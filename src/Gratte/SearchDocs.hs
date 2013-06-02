module Gratte.SearchDocs (
  searchDocs
  ) where

import Network.HTTP

import System.IO

import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G

searchDocs :: Opt.Options -> String -> IO ()
searchDocs opts queryText = do
  let (G.EsHost esHost) = Opt.esHost opts
  let url = esHost ++ "/gratte/document/_search?q=" ++ queryText
  result <- simpleHTTP $ getRequest url

  case result of
    Left err -> unless (Opt.silent opts) (hPutStrLn stderr $ "An error happened when connecting: " ++ show err)
    Right (Response code _ _ body) -> do
      case code of
        (2,_,_) -> return ()
        (x,y,z) -> unless (Opt.silent opts) (hPutStrLn stderr $ "Failure code returned: " ++ show x ++ show y ++ show z)
      case decode (BS.pack body) of
        Nothing -> putStr $ "parsing failed"
        Just jsonBody -> case fromJSON jsonBody of
          Error err -> putStr $ "parsing failed: " ++ err
          Success (G.SearchResult (G.Hits docs)) -> do
            let fps = map G.filepath docs
            mapM_ putStrLn fps
