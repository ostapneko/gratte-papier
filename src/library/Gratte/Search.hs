module Gratte.Search (
  searchDocs
  , getDocs
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte
import           Control.Arrow

import           Data.Char
import           Data.List.Split (splitOn)
import qualified Data.Text                  as T

import           System.FilePath

import           Network.HTTP
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Gratte.Options
import Gratte.TypeDefs

searchDocs :: String -> Gratte ()
searchDocs queryText = do
  docs <- getDocs queryText
  mapM_ outputDoc docs

getDocs :: String -> Gratte [Document]
getDocs queryText = do
  EsHost h <- getOption esHost
  EsIndex i <- getOption esIndex
  let queryString = "?q=" ++ (urlEncode queryText)
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
  case format of
    CompactFormat -> liftIO . putStrLn $ filepath doc
    DetailedFormat -> liftIO $ do
      mapM_ putStrLn $ docInfo doc
      putStrLn "---------------------"

docInfo :: Document -> [String]
docInfo doc = [
                  "Title: " ++ inferredTitle
                , "Path: " ++ path
                , "Tags: " ++ tagString
                , "\nScanned text extract: \n" ++ scannedText
              ]
  where path = filepath doc
        inferredTitle = (takeBaseName
                      >>> reverse
                      >>> dropWhile isAlphaNum
                      >>> drop 1
                      >>> reverse
                      >>> splitOn "-"
                      >>> map capitalize
                      >>> unwords) path
        tagString = unwords . map toText $ tags doc
        scannedText = take 200 . T.unpack $ freeText doc

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
