module Gratte.Utils (
  getFilesRecurs
  ) where

import Data.List

import System.Directory

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
  let notDotDir = not . isPrefixOf "."
  let childrenBaseNames = filter notDotDir dirContents
  let children = zipWith (++) (repeat $ f ++ "/") childrenBaseNames
  return children
