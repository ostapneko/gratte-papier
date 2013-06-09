module Gratte.Search (
  searchDocs
  ) where

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
  EsHost h <- getOption esHost
  let queryString = "?q=" ++ (urlEncode queryText)
  let url = h ++ "/gratte/document/_search" ++ queryString
  logDebug $ "Querying for '" ++ queryText ++ "'"
  result <- liftIO . simpleHTTP $ getRequest url

  case result of
    Left err -> logError $ "An error happened when connecting to ES: " ++ show err
    Right (Response code _ _ body) -> do
      case code of
        (2,_,_) -> return ()
        (x,y,z) -> logError $ "ES failure code returned: " ++ show x ++ show y ++ show z
      case decode (BS.pack body) of
        Nothing -> logError $ "Parsing of the results from ES failed"
        Just jsonBody -> case fromJSON jsonBody of
          Error err -> logError $ "JSON object parsing failed: " ++ err
          Success (SearchResult (Hits docs)) -> do
            mapM_ outputDoc docs

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
