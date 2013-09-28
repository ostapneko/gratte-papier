module Gratte.Add
  ( archive
  , sendToES
  , createDocuments
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import Data.Char

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
  hash      <- liftIO getHash
  DocumentTitle title'    <- getOption title
  gratteDir <- getOption folder
  tags'     <- getOption tags
  mText     <- extractText path
  let titleWithPage = case mPage of
        Nothing -> title'
        Just page -> title' ++ " (Page " ++ show page ++ ")"
      prefix = makePrefix (DocumentTitle titleWithPage)
      docPath = toNestedFilePath gratteDir hash prefix path
  return $ Document {
             docHash        = hash
           , docTitle       = DocumentTitle titleWithPage
           , docFilepath    = DocumentPath docPath
           , docTags        = tags'
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

toNestedFilePath :: GratteFolder
                 -> DocumentHash
                 -> Prefix
                 -> FilePath
                 -> FilePath
toNestedFilePath (GratteFolder dir) (DocumentHash hash) (Prefix prf) f =
  let ext          = takeExtension f
      (a:b:c:rest) = hash
  in dir
     </> [a] </> [b] </> [c]
     </> prf ++ "-" ++ rest ++ ext

copyToRepo :: FilePath -- ^ The path to the original file
           -> Document -- ^ The 'Document' representation of the file
           -> Gratte ()
copyToRepo file doc = do
  let DocumentPath newFile = docFilepath doc
  let dir = takeDirectory newFile
  logDebug $ "\tCopy " ++ file ++ " to " ++ newFile
  isDryRun <- getOption dryRun
  unless isDryRun $ liftIO $ do
    createDirectoryIfMissing True dir
    copyFile file newFile
    let metadataFile = replaceExtension newFile "metadata"
    appendFile metadataFile $ show doc

sendToES :: Document -> Gratte ()
sendToES doc = do
  (EsHost h)  <- getOption esHost
  (EsIndex i) <- getOption esIndex
  isDryRun    <- getOption dryRun
  let (DocumentHash docId) = docHash doc
  let url                  = h </> i </> "document" </> docId
  let payload              = toPayload doc
  logDebug $ "\tSending payload: " ++ toPayload doc
  unless isDryRun $ do
    _ <- liftIO . simpleHTTP $ postRequestWithBody url "application/json" payload
    return ()

logStartAddingFiles :: [Document] -> Gratte ()
logStartAddingFiles docs = do
  logDebug $ "Adding files \n\t"
          ++ L.intercalate "\n\t" (map (documentPathToString . docFilepath) docs)
  logNotice $ "Starting..."

logDoneAddingFiles :: [Document] -> Gratte ()
logDoneAddingFiles docs = logNotice $
  "Done adding " ++ show (length docs) ++ " files."
