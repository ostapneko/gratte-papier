import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Gratte

import           System.Console.GetOpt
import           System.Environment

import qualified Gratte.Options  as O
import qualified Gratte.TypeDefs as G
import qualified Gratte.Add      as Add
import qualified Gratte.Search   as Search
import qualified Gratte.Reindex  as Reindex

main :: IO ()
main = do
  args <- getArgs
  let (actions, params, errors) = getOpt Permute O.options args
  opts <- foldl (>>=) O.defaultOptions actions
  return ()

  flip gratte opts $ do
    setupLogger
    mode <- getOption O.mode
    case errors of
      [] -> case mode of
              G.AddMode     -> addDocs params
              G.ReindexMode -> Reindex.reindex
              G.QueryMode   -> Search.searchDocs $ unwords params
      _ -> mapM_ logCritical errors

addDocs :: [String] -> Gratte ()
addDocs params = do
  content <- liftIO getContents
  let files = lines content
  let tags = map G.Tag params
  Add.addDocuments tags files
