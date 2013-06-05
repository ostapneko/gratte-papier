module Gratte.Reindex (
  reindex
  ) where

import Network.HTTP
import Network.URI

import Data.Maybe

import System.Directory

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import           Gratte.Logger

reindex :: Opt.Options -> IO ()
reindex opts = do
  deleteIndex opts
  importDocs opts

deleteIndex :: Opt.Options -> IO ()
deleteIndex opts = do
  let (G.EsHost esHost) = Opt.esHost opts
  let url = esHost ++ "/gratte/document/"
  let uri = fromJust $ parseURI url
  result <- simpleHTTP $ mkRequest DELETE uri
  case result of
    Right (Response (2, _, _) _ _ body) -> logMsg NOTICE $ "Index deleted: " ++ body
    _                                   -> logMsg ERROR "Something went wrong in the index deletion. Is it already deleted?"

importDocs :: Opt.Options -> IO ()
importDocs opts = do
  let folder = Opt.folder opts
  files <- getFilesRecurs folder
  mapM_ (logMsg DEBUG) files

getFilesRecurs :: FilePath -> IO [FilePath]
getFilesRecurs f = do
  isFile <- doesFileExist f
  case isFile of
    True  -> return [f]
    False -> do
      children <- getDirectoryContents' f
      grandChildren <- mapM getFilesRecurs children
      return $ concat grandChildren

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' f = do
  dirContents <- getDirectoryContents f
  let notDotDir = not . all (=='.')
  let childrenBaseNames = filter notDotDir dirContents
  let children = zipWith (++) (repeat $ f ++ "/") childrenBaseNames
  return children
