module Gratte.Search (
  searchDocs
  ) where

import Control.Monad.Trans
import Control.Monad.Gratte

import Network.HTTP

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Gratte.Options  as O
import qualified Gratte.TypeDefs as G

searchDocs :: String -> Gratte ()
searchDocs queryText = do
  G.EsHost esHost <- getOption O.esHost
  let queryString = "?q=" ++ (urlEncode queryText)
  let url = esHost ++ "/gratte/document/_search" ++ queryString
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
          Success (G.SearchResult (G.Hits docs)) -> do
            let fps = map G.filepath docs
            mapM_ (liftIO . putStrLn) fps
