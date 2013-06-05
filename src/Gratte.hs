import Control.Monad

import System.Console.GetOpt
import System.Environment
import System.Log.Logger
import System.Log.Handler.Simple

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import qualified Gratte.Add      as Add
import qualified Gratte.Search   as Search
import qualified Gratte.Reindex  as Reindex
import           Gratte.Logger

main :: IO ()
main = do
  (args, opts, errors) <- processArgs

  case errors of
    [] -> case Opt.mode opts of
            G.AddMode     -> addDocs opts args
            G.ReindexMode -> Reindex.reindex opts
            G.QueryMode   -> Search.searchDocs opts (unwords args)
    _ -> mapM_ (logMsg opts CRITICAL) errors

processArgs :: IO ([String], Opt.Options, [String])
processArgs = do
  args    <- getArgs
  defOpts <- Opt.defaultOptions
  let (actions, argStrings, errors) = getOpt Permute Opt.options args
  opts <- foldl (>>=) (return defOpts) actions
  configureLogger opts
  return (argStrings, opts, errors)

addDocs :: Opt.Options -> [String] -> IO ()
addDocs opts args = do
  files <- liftM lines getContents
  let tags = map G.Tag args
  Add.addDocuments opts tags files
