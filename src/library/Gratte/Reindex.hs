module Gratte.Reindex (
  reindex
  , getHash
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import Data.Maybe
import Data.Char

import System.FilePath

import Network.HTTP
import Network.URI

import Gratte.Options
import Gratte.Document
import Gratte.Add           (sendToES)
import Gratte.Utils         (getFilesRecurs)

reindex :: Gratte ()
reindex = do
  deleteIndex
  importFiles

deleteIndex :: Gratte ()
deleteIndex = do
  EsHost h <- getOption esHost
  let url = h </> "gratte" </> "document"
  let uri = fromJust $ parseURI url
  result <- liftIO . simpleHTTP $ mkRequest DELETE uri
  case result of
    Right (Response (2, _, _) _ _ body) -> logNotice $ "Index deleted: " ++ body
    _                                   -> logError "Something went wrong in the index deletion. Is it already deleted?"

importFiles :: Gratte ()
importFiles = do
  GratteFolder f <- getOption folder
  logNotice $ "Starting file imports from folder: " ++ f ++ " ..."
  files <- liftIO $ (filter notMetadata) `liftM` getFilesRecurs f
  mapM_ importFile files
  logNotice $ show (length files) ++ " files imported successfully."

notMetadata :: FilePath -> Bool
notMetadata file = takeExtension file /= ".metadata"

importFile :: FilePath -> Gratte ()
importFile file = do
  logDebug $ "Importing File " ++ file
  mDoc <- createDoc file
  case mDoc of
    Just doc -> sendToES doc
    _        -> return ()

createDoc :: FilePath -> Gratte (Maybe Document)
createDoc file = do
  let metaDataFile = replaceExtension file "metadata"
  metadata <- liftIO $ readFile metaDataFile
  let parsed = reads metadata :: [(Document, String)]
  case parsed of
    [(doc, _)] -> return $ Just doc
    _          -> do
      logCritical $ "Could not parse the document " ++ file
      return Nothing

-- foo/bar/a/2/3/4/doc-5678 -> a2345678
getHash :: FilePath -> String
getHash fp =
  let (fileName:dirs) = take 4 . reverse . splitDirectories . dropExtension $ fp
      fileNameNoPrefix = reverse . takeWhile isAlphaNum . reverse $ fileName
  in  concat $ reverse dirs ++ [fileNameNoPrefix]
