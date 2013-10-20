module Gratte.Utils
  ( getFilesRecurs
  , (<//>)
  ) where

import Data.Monoid

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

getFilesRecurs :: FS.FilePath -> IO [FS.FilePath]
getFilesRecurs f = do
  isFile' <- FS.isFile f
  case isFile' of
    True  -> return [f]
    False -> do
      children <- FS.listDirectory f
      let children' = filter (not . null . FS.encodeString . FS.basename) children
      grandChildren <- mapM getFilesRecurs children'
      return $ concat grandChildren

-- | Like Filesystem.Path.append, but remove the second path's
-- first character if it's a \'/\'
(<//>) :: FS.FilePath -> FS.FilePath -> FS.FilePath
f1 <//> f2 = f1 <> f2'
  where f2' = FS.decodeString . dropWhile (=='/') . FS.encodeString $ f2
