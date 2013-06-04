import Control.Monad

import System.Console.GetOpt
import System.Environment

import qualified Gratte.Options    as Opt
import qualified Gratte.TypeDefs   as G
import qualified Gratte.AddDoc     as AddDoc
import qualified Gratte.SearchDocs as SearchDocs

main :: IO ()
main = do
  (args, opts, errors) <- processArgs
  case errors of
    [] -> case Opt.mode opts of
            G.AddMode -> do
              files <- liftM lines getContents
              let tags = map G.Tag args
              AddDoc.addDocuments opts tags files
            G.ReindexMode -> putStrLn "TODO"
            G.QueryMode -> SearchDocs.searchDocs opts (unwords args)
    _ -> mapM_ putStr errors

processArgs :: IO ([String], Opt.Options, [String])
processArgs = do
  args    <- getArgs
  defOpts <- Opt.defaultOptions
  let (actions, argStrings, errors) = getOpt Permute Opt.options args
  opts <- foldl (>>=) (return defOpts) actions
  return (argStrings, opts, errors)
