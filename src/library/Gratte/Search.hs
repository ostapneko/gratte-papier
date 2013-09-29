{-# LANGUAGE OverloadedStrings #-}

module Gratte.Search (
  searchDocs
  , getDocs
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           System.FilePath

import           Network.HTTP
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Gratte.Options
import Gratte.Document

searchDocs :: String -> Gratte ()
searchDocs queryText = do
  docs <- getDocs queryText
  mapM_ outputDoc docs

getDocs :: String -> Gratte [Document]
getDocs queryText = do
  EsHost h  <- getOption esHost
  EsIndex i <- getOption esIndex
  size      <- getOption resultSize
  let queryString = "?q=" ++ (urlEncode queryText) ++ "&size=" ++ (show size)
  let url = h </> i </> "document" </> "_search" ++ queryString
  logDebug $ "Querying for '" ++ queryText ++ "'"
  result <- liftIO . simpleHTTP $ getRequest url

  case result of
    Left err -> logAndReturnEmpty $ "An error happened when connecting to ES: " ++ show err
    Right (Response (x, y, z) _ _ body) -> do
      unless (x == 2) $ do
        logError $ "ES failure code returned: " ++ show x ++ show y ++ show z
      case decode (BS.pack body) of
        Nothing -> logAndReturnEmpty $ "Parsing of the results from ES failed"
        Just jsonBody -> case fromJSON jsonBody of
          Error err -> logAndReturnEmpty $ "JSON object parsing failed: " ++ err
          Success (SearchResult (Hits docs)) -> return docs

logAndReturnEmpty :: String -> Gratte [Document]
logAndReturnEmpty msg = do
  logError msg
  return []

outputDoc :: Document -> Gratte ()
outputDoc doc = do
  format <- getOption outputFormat
  liftIO $ case format of
    CompactFormat -> putStrLn . docPathToString . docPath $ doc
    DetailedFormat -> do
      putStrLn $ describeDoc doc
      putStrLn "---------------------"
