import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import qualified Data.List             as L

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Gratte.Options
import qualified Gratte.Add             as Add
import qualified Gratte.Search          as Search
import qualified Gratte.Reindex         as Reindex

main :: IO ()
main = do
  args <- getArgs
  let (actions, params, errors) = getOpt Permute options args
  defaultOptions' <- defaultOptions
  eOpts <- runEitherT $  L.foldl' (>>=) (right defaultOptions') actions
  case eOpts of
    Right opts -> withGratte opts $ do
      setupLogger
      case (params, errors) of
        ("add":files, []) -> Add.addDocuments files
        (["reindex"], []) -> Reindex.reindex
        ([]         , []) -> liftIO usage
        (files      , []) -> Search.searchDocs $ unwords files
        _                 -> mapM_ logCritical errors
    Left UsageWithSuccess -> usage >> exitSuccess
    Left (InvalidOptions msg) -> do
      hPutStrLn stderr $ "Error while parsing the options: " ++ msg
      usage
      exitFailure
