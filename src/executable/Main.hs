import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           System.Console.GetOpt
import           System.Environment

import           Gratte.Options
import           Gratte.Document
import qualified Gratte.Add             as Add
import qualified Gratte.Search          as Search
import qualified Gratte.Reindex         as Reindex

main :: IO ()
main = do
  args <- getArgs
  let (actions, params, errors) = getOpt Permute options args
  opts <- foldl (>>=) defaultOptions actions

  withGratte opts $ do
    setupLogger
    case (params, errors) of
      ("add":files, []) -> Add.addDocuments files
      (["reindex"], []) -> Reindex.reindex
      (files      , []) -> Search.searchDocs $ unwords files
      (_          , []) -> liftIO usage
      _                 -> mapM_ logCritical errors
