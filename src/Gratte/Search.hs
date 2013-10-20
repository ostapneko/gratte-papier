{-# LANGUAGE OverloadedStrings #-}

module Gratte.Search
  ( searchDocs
  , getDocs
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Filesystem.Path.CurrentOS as FS

import           Network.HTTP
import           Network.URI

import           Gratte.Document
import           Gratte.Options
import           Gratte.Utils

searchDocs :: String -> Gratte ()
searchDocs queryText = do
  docs <- getDocs queryText
  mapM_ outputDoc docs

getDocs :: String -> Gratte [Document]
getDocs queryText = do
  EsHost h  <- getOption esHost
  EsIndex i <- getOption esIndex
  size      <- getSearchOption resultSize
  let uriPath' = "/" ++ i ++ "/document/_search"
  let queryString = "?q=" ++ (urlEncode queryText) ++ "&size=" ++ (show size)

  let uri = h { uriPath = uriPath' , uriQuery = queryString }

  logDebug $ "Querying for '" ++ queryText ++ "'"
  result <- liftIO . simpleHTTP $ mkRequest GET uri

  case result of
    Left err -> logAndReturnEmpty $ "An error happened when connecting to ES: " ++ show err
    Right (Response (x, y, z) _ _ body) -> do
      unless (x == 2) $ do
        logError $ "ES failure code returned: " ++ show x ++ show y ++ show z
      case decode (BS.pack body) of
        Nothing       -> logAndReturnEmpty $ "Parsing of the results from ES failed"
        Just jsonBody -> case fromJSON jsonBody of
          Error err -> logAndReturnEmpty $ "JSON object parsing failed: " ++ err ++ ". Original body: " ++ body
          Success (SearchResult (Hits docPayloads)) -> return $ map (\ (DocumentPayload doc) -> doc) docPayloads

logAndReturnEmpty :: String -> Gratte [Document]
logAndReturnEmpty msg = do
  logError msg
  return []

outputDoc :: Document -> Gratte ()
outputDoc doc = do
  format <- getSearchOption outputFormat
  docFolder <- getOption folder
  liftIO $ case format of
    OutputFormatCompact  -> do
      let DocumentPath relPath = docPath doc
          fullPath = docFolder <//> relPath
      putStrLn $ FS.encodeString fullPath
    OutputFormatDetailed -> do
      putStrLn $ describeDoc docFolder doc
