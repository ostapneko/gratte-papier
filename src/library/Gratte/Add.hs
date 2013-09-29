module Gratte.Add
  ( archive
  , sendToES
  , createDocuments
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import Data.Aeson
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

import System.FilePath
import System.Directory
import System.Time

import Network.HTTP

import qualified Data.List     as L
import           Data.Hash.MD5

import Gratte.Options
import Gratte.Document
import Gratte.TextExtractor (extractText)

-- | Add several documents with the same tags
archive :: [(FilePath, Document)] -> Gratte ()
archive pairs = do
  let docs = map snd pairs
  logStartAddingFiles docs
  forM_ pairs $ \ (file, doc) -> processFile file doc
  logDoneAddingFiles docs

-- | Add a single file with multiple tags
processFile :: FilePath -> Document -> Gratte ()
processFile f doc = do
  -- copy file
  copyToRepo f doc
  -- send to ElasticSearch
  sendToES doc

-- | Create a list of docuements from filepaths
-- If there are more than one document, their title
-- and file name contain the page number
createDocuments :: [FilePath] -> Gratte [Document]
createDocuments [path] = do doc <- createDocument Nothing path; return [doc]
createDocuments paths  = do
  let pageAndPath = zip [1..] paths
  forM pageAndPath $ \ (page, path) -> createDocument (Just page) path

-- | Create a document. It doesn't persist it.
createDocument :: Maybe Int -- ^ The page number
               -> FilePath  -- ^ The path to the original document
               -> Gratte Document
createDocument mPage path = do
  opts      <- getOptions
  hash      <- liftIO getHash
  gratteDir <- getOption folder
  mText     <- extractText path
  let titleString   = either (error "Options should have a title") docTitleToString (title opts)
  let sender'       = either (error "Options should have a sender") id (sender opts)
  let recipient'    = either (error "Options should have a recipient") id (recipient opts)
  let titleWithPage = case mPage of
        Nothing -> titleString
        Just page -> titleString ++ " (Page " ++ show page ++ ")"
      prefix = makePrefix (DocumentTitle titleWithPage)
      targetPath  = toTargetPath gratteDir hash prefix path
  return $ Document {
             docHash        = hash
           , docTitle       = DocumentTitle titleWithPage
           , docPath        = DocumentPath targetPath
           , docSender      = sender'
           , docRecipient   = recipient'
           , docDate        = date opts
           , docTags        = tags opts
           , docScannedText = mText
           }

makePrefix :: DocumentTitle -> Prefix
makePrefix (DocumentTitle t) =
    let prf =  filter isAscii . map (replaceSpaces . toLower) $ t
    in Prefix prf
  where replaceSpaces c = if (isSpace c) then '-' else c

getHash :: IO DocumentHash
getHash = do
  TOD s ps <- getClockTime
  let timeStamp = show (s * 1000000000000 + ps)
  return $ DocumentHash $ md5s $ Str timeStamp

toTargetPath :: GratteFolder
             -> DocumentHash
             -> Prefix
             -> FilePath
             -> FilePath
toTargetPath (GratteFolder dir) (DocumentHash hash) (Prefix prf) f =
  let ext          = takeExtension f
      (a:b:c:rest) = hash
  in dir
     </> [a] </> [b] </> [c]
     </> prf ++ "-" ++ rest ++ ext

copyToRepo :: FilePath -- ^ The path to the original file
           -> Document -- ^ The 'Document' representation of the file
           -> Gratte ()
copyToRepo file doc = do
  let DocumentPath newFile = docPath doc
  let dir = takeDirectory newFile
  logDebug $ "\tCopy " ++ file ++ " to " ++ newFile
  liftIO $ do
    createDirectoryIfMissing True dir
    copyFile file newFile
    let metadataFile = replaceExtension newFile "json"
    BS.writeFile metadataFile $ encode doc

sendToES :: Document -> Gratte ()
sendToES doc = do
  (EsHost h)  <- getOption esHost
  (EsIndex i) <- getOption esIndex
  let (DocumentHash docId) = docHash doc
  let url                  = h </> i </> "document" </> docId
  let payload              = toPayload doc
  logDebug $ "\tSending payload: " ++ toPayload doc
  _ <- liftIO . simpleHTTP $ postRequestWithBody url "application/json" payload
  return ()

logStartAddingFiles :: [Document] -> Gratte ()
logStartAddingFiles docs = do
  logDebug $ "Adding files \n\t"
          ++ L.intercalate "\n\t" (map (docPathToString . docPath) docs)
  logNotice $ "Starting..."

logDoneAddingFiles :: [Document] -> Gratte ()
logDoneAddingFiles docs = logNotice $
  "Done adding " ++ show (length docs) ++ " files."
