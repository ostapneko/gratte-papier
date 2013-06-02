import Control.Monad

import System.Console.GetOpt
import System.Environment

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import qualified Gratte.AddDoc   as AddDoc

main :: IO ()
main = do
  (tags, opts, errors) <- processArgs
  case errors of
    [] -> case Opt.mode opts of
            G.AddMode -> do
              files <- liftM lines getContents
              AddDoc.addDocuments opts tags files
            G.QueryMode -> print $ "query mode"
    _ -> mapM_ putStr errors

processArgs :: IO ([G.Tag], Opt.Options, [String])
processArgs = do
  args    <- getArgs
  defOpts <- Opt.defaultOptions
  let (actions, tagStrings, errors) = getOpt Permute Opt.options args
  opts <- foldl (>>=) (return defOpts) actions
  let tags = map G.Tag tagStrings
  return (tags, opts, errors)
