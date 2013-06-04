module Gratte.Options (
  module Gratte.Options
  ) where

import System.IO
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.Directory

import Data.Char

import Gratte.TypeDefs

data Options = Options {
    verbose :: Bool
  , silent  :: Bool
  , mode    :: Mode
  , esHost  :: EsHost
  , prefix  :: Prefix
  , folder  :: FilePath
  , dryRun  :: Bool
  , ocr     :: Bool
}

defaultOptions :: IO Options
defaultOptions = do
  homeDir <- getHomeDirectory
  let defaultFolder = homeDir ++ "/.gratte"
  return Options {
    verbose = False
  , silent  = False
  , mode    = QueryMode
  , esHost  = EsHost "http://localhost:9200"
  , prefix  = Prefix "doc"
  , folder  = defaultFolder
  , dryRun  = False
  , ocr     = False
}

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

    , Option "e" ["es-host"]
             (ReqArg (\arg opts -> return opts { esHost = EsHost arg }) "HOST")
             "Elastic search host and port, defaults to http://localhost:9200"

    , Option "m" ["mode"]
             (ReqArg (\arg opts -> return opts { mode = getMode arg }) "query|add|reindex")
             ("In query mode (default), find the docs which match the args\n" ++
             "In add mode, add the docs taken in stdin, and tag them with the args\n" ++
             "In reindex mode, delete ES index and reingest the files in the gratte folder into ES")

    , Option "p" ["prefix"]
             (ReqArg (\arg opts -> return opts { prefix = Prefix arg }) "PREFIX")
             "Prefixes the files with the prefix argument. Defaults to 'doc'"

    , Option "f" ["folder"]
             (ReqArg (\arg opts -> return opts { folder = arg }) "OUTPUT FOLDER")
             "The output folder. Defaults to ~/.gratte"

    , Option "d" ["dry-run"]
             (NoArg (\opts -> return opts { dryRun = True }))
             "Run in dry mode: no files are copied and the payloads that would have been sent to ES are displayed to stdout"

    , Option "o" ["ocr"]
             (NoArg (\opts -> return opts { ocr = True }))
             "Uses OCR to try extract the text from the documents and add it as searchable metadata. Requires tesseract to be installed."
  ]

getMode :: String -> Mode
getMode m = case map toLower m of
              "add"     -> AddMode
              "reindex" -> ReindexMode
              _         -> QueryMode

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] [tags]\n\n" ++
               "In query mode (default), it will output a list of files matching the string given as arguments\n\n" ++
               "In add mode (with the -a flag), the files given in the stdin will be tagged with the command's arguments\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options
