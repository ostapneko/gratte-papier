import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           System.Console.GetOpt
import           System.Environment

import Gratte.Options
import Gratte.Document
import qualified Gratte.Add      as Add
import qualified Gratte.Search   as Search
import qualified Gratte.Reindex  as Reindex

main :: IO ()
main = do
  args <- getArgs
  let (actions, params, errors) = getOpt Permute options args
  opts <- foldl (>>=) defaultOptions actions
  return ()

  flip gratte opts $ do
    setupLogger
    m <- getOption mode
    case errors of
      [] -> case m of
              AddMode     -> addDocs params
              ReindexMode -> Reindex.reindex
              QueryMode   -> Search.searchDocs $ unwords params
      _ -> mapM_ logCritical errors

addDocs :: [String] -> Gratte ()
addDocs params = do
  content <- liftIO getContents
  let files = lines content
  let ts = map Tag params
  Add.addDocuments ts files
