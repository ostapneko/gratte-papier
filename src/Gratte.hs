import Control.Monad

import System.Console.GetOpt
import System.Environment

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import qualified Gratte.AddDoc   as AddDoc

main :: IO ()
main = do
  args <- getArgs
  defOpts <- Opt.defaultOptions
  let (actions, tagStrings, errors) = getOpt Permute Opt.options args
  opts <- foldl (>>=) (return defOpts) actions
  let tags = map G.Tag tagStrings

  case errors of
    [] -> case Opt.mode opts of
            G.AddMode -> do
              files <- liftM lines getContents
              AddDoc.addDocuments opts tags files
            G.QueryMode -> print $ "query mode" ++ show tagStrings
    _ -> mapM_ putStr errors
