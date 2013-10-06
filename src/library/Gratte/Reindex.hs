module Gratte.Reindex
  ( reindex
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

import System.FilePath
import System.Directory

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
  EsIndex i <- getOption esIndex
  let url = h </> i </> "document"
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
  mapM_ (liftIO . putStrLn) files
  mapM_ importFile files
  logNotice $ show (length files) ++ " files imported successfully."

notMetadata :: FilePath -> Bool
notMetadata file = takeExtension file /= ".json"

importFile :: FilePath -> Gratte ()
importFile file = do
  logDebug $ "Importing File " ++ file
  mDoc <- createDoc file
  case mDoc of
    Just doc -> sendToES doc
    _        -> return ()

createDoc :: FilePath -> Gratte (Maybe Document)
createDoc file = do
  let metadataFile = replaceExtension file "json"
  metadataExist <- liftIO $ doesFileExist metadataFile
  if (metadataExist)
    then do
      jsonDoc <- liftIO $ BS.readFile metadataFile
      liftIO $ print jsonDoc
      let parsed = decode jsonDoc :: Maybe Document
      case parsed of
        Just doc -> return $ Just doc
        _          -> do
          logCritical $ "Could not parse the document " ++ metadataFile
          return Nothing
    else do
      logError $ "Could not find metadata for file: " ++ file
      return Nothing
