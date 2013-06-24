module Gratte.Options (
  module Gratte.Options
  ) where

import System.IO
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.Directory

import Data.Char

newtype EsHost  = EsHost String
newtype EsIndex = EsIndex String
newtype Prefix  = Prefix String

data Mode         = AddMode | QueryMode | ReindexMode
data PDFMode      = NoPDFMode | ImagePDFMode | TextPDFMode
data OutputFormat = CompactFormat | DetailedFormat

data Options = Options {
    verbose      :: Bool
  , silent       :: Bool
  , mode         :: Mode
  , esHost       :: EsHost
  , esIndex      :: EsIndex
  , prefix       :: Prefix
  , folder       :: FilePath
  , dryRun       :: Bool
  , ocr          :: Bool
  , logFilePath  :: FilePath
  , outputFormat :: OutputFormat
  , pdfMode      :: PDFMode
}

defaultOptions :: IO Options
defaultOptions = do
  homeDir <- getHomeDirectory
  let defaultFolder = homeDir ++ "/.gratte"
  return Options {
    verbose      = False
  , silent       = False
  , mode         = QueryMode
  , esHost       = EsHost "http://localhost:9200"
  , esIndex      = EsIndex "gratte"
  , prefix       = Prefix "doc"
  , folder       = defaultFolder
  , dryRun       = False
  , ocr          = False
  , logFilePath  = "/var/log/gratte/gratte.log"
  , outputFormat = DetailedFormat
  , pdfMode      = NoPDFMode
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

    , Option "" ["es-index"]
             (ReqArg (\arg opts -> return opts { esIndex = EsIndex arg }) "INDEX")
             "Elastic search index, defaults to 'gratte'"

    , Option "m" ["mode"]
             (ReqArg (\arg opts -> do m <- getMode arg; return opts { mode = m }) "q[uery]|a[dd]|r[eindex]")
             ("In query mode (default), find the docs which match the args\n" ++
             "In add mode, add the docs taken in stdin, and tag them with the args\n" ++
             "In reindex mode, delete ES index and reingest the files in the gratte folder into ES")

    , Option "t" ["title"]
             (ReqArg (\arg opts -> return opts { prefix = Prefix arg }) "TITLE")
             "Prefixes the files with the title argument. Defaults to 'doc'. Try to use this-kind-of-case."

    , Option "" ["folder"]
             (ReqArg (\arg opts -> return opts { folder = arg }) "OUTPUT FOLDER")
             "The output folder. Defaults to ~/.gratte"

    , Option "d" ["dry-run"]
             (NoArg (\opts -> return opts { dryRun = True }))
             "Run in dry mode: no files are copied and the payloads that would have been sent to ES are displayed to stdout"

    , Option "o" ["ocr"]
             (NoArg (\opts -> return opts { ocr = True }))
             "Uses OCR to try extract the text from the documents and add it as searchable metadata. Requires tesseract to be installed."

    , Option "" ["log--file"]
             (ReqArg (\arg opts -> return opts { logFilePath = arg }) "PATH")
             "The log file. Defaults to /var/log/gratte/gratte.log"

    , Option "f" ["format"]
             (ReqArg (\arg opts -> do f <- getFormat arg; return opts { outputFormat = f }) "c[ompact]|d[etail]")
             "The output format in query mode. 'compact' will spit the file paths. 'detail' spits results in human-readable format. Defaults to 'detail'."
    , Option "p" ["pdf-mode"]
             (ReqArg (\arg opts -> do m <- getPDFMode arg; return opts { pdfMode = m}) "i[mage]|t[text]")
             "The text recognition mode for PDF files, when used in conjonction with -o. '-p image' will consider the pdf as an image, while '-p text' will treat the PDF as text. This option is mandatory if you are scanning at least one PDF file with OCR."
  ]

getMode :: String -> IO Mode
getMode m
  | map toLower m `elem` ["add", "a"]     = return AddMode
  | map toLower m `elem` ["reindex", "r"] = return ReindexMode
  | map toLower m `elem` ["query", "q"]   = return QueryMode
  | otherwise = do
      hPutStr stderr "Allowed value for -m : a[dd], r[eindex], q[uery]"
      exitFailure

getFormat :: String -> IO OutputFormat
getFormat f
  | map toLower f `elem` ["compact", "c"] = return CompactFormat
  | map toLower f `elem` ["detail", "d"]  = return DetailedFormat
  | otherwise = do
      hPutStr stderr "Allowed value for -f : c[ompact], d[etail]"
      exitFailure

getPDFMode :: String -> IO PDFMode
getPDFMode m
  | map toLower m `elem` ["image", "i"] = return ImagePDFMode
  | map toLower m `elem` ["text", "t"]  = return TextPDFMode
  | otherwise = do
      hPutStr stderr "Allowed value for -p : i[mage], t[ext]"
      exitFailure

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] [tags]\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options
