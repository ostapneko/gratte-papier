import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Gratte.TypeDefs

options :: [OptDescr (Options -> IO Options)]
options = [
      Option "V" ["verbose"]
             (NoArg (\opts -> return opts { verbose = True, silent = False }))
             "Verbose mode"

    , Option "s" ["silent"]
             (NoArg (\opts -> return opts { silent = True, verbose = False }))
             "Silent mode"

    , Option "h" ["help"]
             (NoArg (\_ -> usage >> exitWith ExitSuccess))
             "Show help"
  ]

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] [tags]\n\n" ++
               "The files given in the stdin will be tagged with the command's arguments\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options

main :: IO ()
main = do
  args <- getArgs
  let (actions, tags, errors) = getOpt Permute options args
  _ <- foldl (>>=) (return defaultOptions) actions

  case errors of
    [] -> do
      mapM_ putStrLn tags
    _ -> mapM_ putStr errors
