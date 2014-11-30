{-# LANGUAGE OverloadedStrings #-}

module Gratte.Add
  ( archive
  , sendToES
  , createDocuments
  ) where

import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans

import           Data.Aeson
import           Data.Char
import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

import           System.Time

import qualified Filesystem                 as FS
import qualified Filesystem.Path.CurrentOS  as FS

import           Network.HTTP
import           Network.URI

import           Data.Hash.MD5
import qualified Data.List                  as L

import           Gratte.Document
import           Gratte.Options
import           Gratte.TextExtractor (extractText)
import           Gratte.Utils

-- | Add several documents with the same tags
archive :: [(FS.FilePath, Document)] -> Gratte ()
archive pairs = do
  let docs = map snd pairs
  logStartAddingFiles docs
  forM_ pairs (uncurry processFile)
  logDoneAddingFiles docs

-- | Add a single file with multiple tags
processFile :: FS.FilePath -> Document -> Gratte ()
processFile f doc = do
  -- copy file
  copyToRepo f doc
  -- send to ElasticSearch
  sendToES doc

-- | Create a list of documents from filepaths
-- If there are more than one document, their title
-- and file name contain the page number
createDocuments :: [FS.FilePath] -> Gratte [Document]
createDocuments [file] = do
  doc <- createDocument Nothing file
  return [doc]
createDocuments paths  = do
  let pageAndPath = zip [1..] paths
  forM pageAndPath $ \ (page, file) -> createDocument (Just page) file

-- | Create a document. It doesn't persist it.
createDocument :: Maybe Int -- ^ The page number
               -> FS.FilePath  -- ^ The path to the original document
               -> Gratte Document
createDocument mPage file = do
  addOpts   <- getAddOptions
  hash      <- liftIO getHash
  mText     <- extractText file
  let DocumentTitle titleString = title addOpts
  let titleWithPage = case mPage of
        Nothing   -> titleString
        Just page -> titleString ++ " (Page " ++ show page ++ ")"
      prefix = makePrefix (DocumentTitle titleWithPage)
      targetPath = toTargetPath hash prefix file
  return Document {
             docHash        = hash
           , docTitle       = DocumentTitle titleWithPage
           , docPath        = DocumentPath targetPath
           , docSender      = sender addOpts
           , docRecipient   = recipient addOpts
           , docDate        = date addOpts
           , docTags        = tags addOpts
           , docScannedText = mText
           }

makePrefix :: DocumentTitle -> String
makePrefix (DocumentTitle t) =
  filter isAscii . map (replaceSpaces . toLower) $ t
    where replaceSpaces c = if isSpace c then '-' else c

getHash :: IO DocumentHash
getHash = do
  TOD s ps <- getClockTime
  let timeStamp = show (s * 1000000000000 + ps)
  return $ DocumentHash $ md5s $ Str timeStamp

toTargetPath :: DocumentHash
             -> String
             -> FS.FilePath
             -> FS.FilePath
toTargetPath (DocumentHash hash) prf f =
  let ext                 = fromMaybe "" $ FS.extension f
      (a:b:c:rest)        = hash
      [a', b', c'] = map FS.decodeString [[a], [b], [c]]
  in "/" <//> a' <//> b' <//> c'
     <//> (FS.decodeString (prf ++ "-" ++ rest) `FS.addExtension` ext)

copyToRepo :: FS.FilePath -- ^ The path to the original file
           -> Document    -- ^ The 'Document' representation of the file
           -> Gratte ()
copyToRepo file doc = do
  let DocumentPath newFileRelPath = docPath doc
  docFolder <- getOption folder
  let newFile = docFolder <//> newFileRelPath
  let dir = FS.directory newFile
  logDebug $ "\tCopy " ++ FS.encodeString file ++ " to " ++ FS.encodeString newFile
  liftIO $ do
    FS.createTree dir
    FS.copyFile file newFile
    let metadataFile = FS.encodeString $ FS.replaceExtension newFile "json"
    BS.writeFile metadataFile $ encode doc

sendToES :: Document -> Gratte ()
sendToES doc = do
  (EsHost h)  <- getOption esHost
  (EsIndex i) <- getOption esIndex
  let (DocumentHash docId) = docHash doc
  let url                  = h { uriPath = i ++ "/document/" ++ docId }
  let payload              = BS.unpack . encode $ DocumentPayload doc
  logDebug $ "\tSending payload: " ++ payload
  let req = setRequestBody (mkRequest POST url) ("application/json", payload)
  _ <- liftIO . simpleHTTP $ req
  return ()

logStartAddingFiles :: [Document] -> Gratte ()
logStartAddingFiles docs = do
  logDebug $ "Adding files \n\t"
          ++ L.intercalate "\n\t" (map (docPathToString . docPath) docs)
  logNotice "Starting..."

logDoneAddingFiles :: [Document] -> Gratte ()
logDoneAddingFiles docs = logNotice $
  "Done adding " ++ show (length docs) ++ " files."
