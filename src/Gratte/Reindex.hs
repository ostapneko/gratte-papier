{-# LANGUAGE OverloadedStrings #-}

module Gratte.Reindex
  ( reindex
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

import           Network.HTTP
import           Network.URI

import           Gratte.Options
import           Gratte.Document
import           Gratte.Add           (sendToES)
import           Gratte.Utils         (getFilesRecurs)

reindex :: Gratte ()
reindex = do
  deleteIndex
  importFiles

deleteIndex :: Gratte ()
deleteIndex = do
  EsHost h <- getOption esHost
  EsIndex i <- getOption esIndex
  let url = h { uriPath = i ++ "/document" }
  result <- liftIO . simpleHTTP $ (mkRequest DELETE url :: Request BS.ByteString)
  case result of
    Right (Response (2, _, _) _ _ _) -> logNotice $ "Index deleted"
    Right (Response (4, 0, 4) _ _ _) -> logNotice $ "No index found"
    _                                -> logError  $ "Something went wrong in the index deletion. :" ++ show result

importFiles :: Gratte ()
importFiles = do
  f <- getOption folder
  logNotice $ "Starting file imports from folder: " ++ FS.encodeString f ++ " ..."
  files <- liftIO $ (filter notMetadata) `liftM` getFilesRecurs f
  mapM_ importFile files
  logNotice $ show (length files) ++ " files imported successfully."

notMetadata :: FS.FilePath -> Bool
notMetadata file = FS.extension file /= Just ".json"

importFile :: FS.FilePath -> Gratte ()
importFile file = do
  logDebug $ "Importing File " ++ FS.encodeString file
  mDoc <- createDoc file
  case mDoc of
    Just doc -> sendToES doc
    _        -> return ()

createDoc :: FS.FilePath -> Gratte (Maybe Document)
createDoc file = do
  let metadataFile = FS.replaceExtension file "json"
  metadataExist <- liftIO $ FS.isFile metadataFile
  if (metadataExist)
    then do
      jsonDoc <- liftIO $ BS.readFile (FS.encodeString metadataFile)
      let parsed = decode jsonDoc :: Maybe Document
      case parsed of
        Just doc -> return $ Just doc
        _          -> do
          logCritical $ "Could not parse the document " ++ FS.encodeString metadataFile
          return Nothing
    else do
      logError $ "Could not find metadata for file: " ++ FS.encodeString file
      return Nothing
