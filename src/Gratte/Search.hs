module Gratte.Search (
  searchDocs
  ) where

import Network.HTTP

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import           Gratte.Logger

searchDocs :: Opt.Options -> String -> IO ()
searchDocs opts queryText = do
  let (G.EsHost esHost) = Opt.esHost opts
  let url = esHost ++ "/gratte/document/_search?q=" ++ queryText
  logMsg DEBUG $ "Querying for '" ++ queryText ++ "'"
  result <- simpleHTTP $ getRequest url

  case result of
    Left err -> logMsg ERROR $ "An error happened when connecting to ES: " ++ show err
    Right (Response code _ _ body) -> do
      case code of
        (2,_,_) -> return ()
        (x,y,z) -> logMsg ERROR $ "ES failure code returned: " ++ show x ++ show y ++ show z
      case decode (BS.pack body) of
        Nothing -> logMsg ERROR $ "Parsing of the results from ES failed"
        Just jsonBody -> case fromJSON jsonBody of
          Error err -> logMsg ERROR $ "JSON object parsing failed: " ++ err
          Success (G.SearchResult (G.Hits docs)) -> do
            let fps = map G.filepath docs
            mapM_ putStrLn fps
