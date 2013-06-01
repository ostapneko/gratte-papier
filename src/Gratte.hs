import System.Console.GetOpt

import System.Environment

import qualified Gratte.Options as O

main :: IO ()
main = do
  args <- getArgs
  let (actions, tags, errors) = getOpt Permute O.options args
  _ <- foldl (>>=) (return O.defaultOptions) actions

  case errors of
    [] -> do
      mapM_ putStrLn tags
    _ -> mapM_ putStr errors
