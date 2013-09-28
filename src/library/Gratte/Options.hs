module Gratte.Options (
  module Gratte.Options
  ) where

import           Control.Monad.Trans.Either

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.IO

import           Data.Char
import qualified Data.List.Split as SPL

import           Gratte.Tag
import           Gratte.Document

newtype EsHost  = EsHost String
newtype EsIndex = EsIndex String
newtype Prefix  = Prefix String
newtype GratteFolder = GratteFolder FilePath

data PDFMode      = NoPDFMode | ImagePDFMode | TextPDFMode
data OutputFormat = CompactFormat | DetailedFormat

data Options = Options
  { verbose      :: Bool
  , silent       :: Bool
  , esHost       :: EsHost
  , esIndex      :: EsIndex
  , title        :: DocumentTitle
  , folder       :: GratteFolder
  , dryRun       :: Bool
  , ocr          :: Bool
  , logFilePath  :: FilePath
  , outputFormat :: OutputFormat
  , pdfMode      :: PDFMode
  , resultSize   :: Int
  , tags         :: [Tag]
  }

defaultOptions :: IO Options
defaultOptions = do
  homeDir <- getHomeDirectory
  let defaultFolder = GratteFolder $  homeDir ++ "/.gratte"
  return Options {
    verbose      = False
  , silent       = False
  , esHost       = EsHost "http://localhost:9200"
  , esIndex      = EsIndex "gratte"
  , title        = DocumentTitle "Doc"
  , folder       = defaultFolder
  , dryRun       = False
  , ocr          = False
  , logFilePath  = "/var/log/gratte/gratte.log"
  , outputFormat = DetailedFormat
  , pdfMode      = NoPDFMode
  , resultSize   = 100
  , tags         = []
  }

data EarlyExit = UsageWithSuccess
               | InvalidOptions String

options :: [OptDescr (Options -> EitherT EarlyExit IO Options)]
options = [
      Option "V" ["verbose"]
             (NoArg (\opts -> return $ opts { verbose = True, silent = False }))
             "Verbose mode"

    , Option "s" ["silent"]
             (NoArg (\opts -> return opts { silent = True, verbose = False }))
             "Silent mode"

    , Option "h" ["help"]
             (NoArg (\_ -> left UsageWithSuccess))
             "Show help"

    , Option "e" ["es-host"]
             (ReqArg (\arg opts -> return opts { esHost = EsHost arg }) "HOST")
             "Elastic search host and port, defaults to http://localhost:9200"

    , Option "" ["es-index"]
             (ReqArg (\arg opts -> return opts { esIndex = EsIndex arg }) "INDEX")
             "Elastic search index, defaults to 'gratte'"

    , Option "t" ["title"]
             (ReqArg (\arg opts -> return opts { title = DocumentTitle arg }) "TITLE")
             "The title of the document"

    , Option "T" ["tags"]
             (ReqArg (\arg opts -> return opts { tags = map (Tag . dropWhile (==' ')) . SPL.splitOneOf ",:" $ arg }) "TAG1,TAG2")
             "Add a comma or colon separated list of tags to the document. Only useful in add mode."

    , Option "" ["folder"]
             (ReqArg (\arg opts -> return opts { folder = GratteFolder arg }) "OUTPUT FOLDER")
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
             (ReqArg handleFormat "c[ompact]|d[etail]")
             "The output format in query mode. 'compact' will spit the file paths. 'detail' spits results in human-readable format. Defaults to 'detail'."
    , Option "p" ["pdf-mode"]
             (ReqArg handlePDFMode "i[mage]|t[text]")
             "The text recognition mode for PDF files, when used in conjonction with -o. '-p image' will consider the pdf as an image, while '-p text' will treat the PDF as text. This option is mandatory if you are scanning at least one PDF file with OCR."

    , Option "n" ["result-size"]
             (ReqArg (\arg opts -> do s <- getResultSize arg; return opts { resultSize = s }) "SIZE")
             "The size of the result list. Defaults to 100."
  ]

getResultSize :: String -> EitherT EarlyExit IO Int
getResultSize s = case reads s of
  [(s', _)] -> return s'
  _         -> left $ InvalidOptions "Need a numeric value for the \"result-size\" option"

usage :: IO ()
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [add file1 file2...|reindex|myquerystring]\n\n" ++
               "Options:"
  hPutStr stderr $ usageInfo header options

handleFormat :: String -> Options -> EitherT EarlyExit IO Options
handleFormat arg opts = do
  let mFormat = case map toLower arg of
        "compact" -> Just CompactFormat
        "c"       -> Just CompactFormat
        "detail"  -> Just DetailedFormat
        "d"       -> Just DetailedFormat
        _         -> Nothing
  case mFormat of
    Just format -> return opts { outputFormat = format }
    _           -> left $ InvalidOptions "Allowed value for -f : c[ompact], d[etail]"

handlePDFMode:: String -> Options -> EitherT EarlyExit IO Options
handlePDFMode arg opts = do
  let mMode = case map toLower arg of
        "image" -> Just ImagePDFMode
        "i"     -> Just ImagePDFMode
        "text"  -> Just TextPDFMode
        "t"     -> Just TextPDFMode
        _       -> Nothing
  case mMode of
    Just mode -> return opts { pdfMode = mode }
    _           -> left $ InvalidOptions "Allowed value for -p : i[mage], t[ext]"
