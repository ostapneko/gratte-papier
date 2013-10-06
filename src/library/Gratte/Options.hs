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
  , folder       :: GratteFolder
  , ocr          :: Bool
  , logFilePath  :: FilePath
  , outputFormat :: OutputFormat
  , pdfMode      :: PDFMode
  , resultSize   :: Int
  -- Document
  , title        :: Either EarlyExit DocumentTitle
  , sender       :: Either EarlyExit DocumentSender
  , recipient    :: Either EarlyExit DocumentRecipient
  , date         :: Maybe DocumentDate
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
  , folder       = defaultFolder
  , ocr          = False
  , logFilePath  = "/var/log/gratte/gratte.log"
  , outputFormat = DetailedFormat
  , pdfMode      = NoPDFMode
  , resultSize   = 100
  -- Document
  , title        = Left $ InvalidOptions "The documents must have a title"
  , sender       = Left $ InvalidOptions "The documents must have a sender"
  , recipient    = Left $ InvalidOptions "The documents must have a recipient"
  , date         = Nothing
  , tags         = []
  }

data EarlyExit = UsageWithSuccess
               | InvalidOptions String
               deriving Show

optionDescrs :: [OptDescr (Options -> EitherT EarlyExit IO Options)]
optionDescrs = [
      Option "V" ["verbose"]
             (NoArg (\opts -> return $ opts { verbose = True, silent = False }))
             "Verbose mode"

    , Option "" ["silent"]
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

    , Option "" ["folder"]
             (ReqArg (\arg opts -> return opts { folder = GratteFolder arg }) "OUTPUT FOLDER")
             "The output folder. Defaults to ~/.gratte"

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

    , Option "t" ["title"]
             (ReqArg (\arg opts -> return opts { title = Right (DocumentTitle arg) }) "\"TITLE\"")
             "The title of the documents. If more than one documents are present, add a page number after it (e.g. \"My doc (Page 1)\", etc. )"

    , Option "s" ["sender"]
             (ReqArg (\arg opts -> return opts { sender = Right (DocumentSender arg) }) "\"NAME\"")
             "The documents' sender"

    , Option "r" ["recipient"]
             (ReqArg (\arg opts -> return opts { recipient = Right (DocumentRecipient arg) }) "\"NAME\"")
             "The documents' recipient (who these documents where addressed to)"

    , Option "d" ["date"]
             (ReqArg handleDate "\"MONTH YEAR\"")
             ("The documents' month (optionaly) and year. If provided, the date MUST be in the form \"September 2013\".")

    , Option "T" ["tags"]
             (ReqArg (\arg opts -> return opts { tags = map (Tag . dropWhile (==' ')) . SPL.splitOneOf ",:" $ arg }) "TAG1,TAG2")
             "Add a comma or colon separated list of tags to the document. Only useful in add mode."
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
  hPutStr stderr $ usageInfo header optionDescrs

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

handleDate :: String -> Options -> EitherT EarlyExit IO Options
handleDate arg opts =
  case parseDate arg of
    Left failure -> left failure
    Right mDate -> return $ opts { date = mDate }

parseDate :: String -> Either EarlyExit (Maybe DocumentDate)
parseDate arg =
  let (lhs, rhs) = (\ (x, y) -> (x, drop 1 y)) . break (==' ') $ arg
      (eMonth, eYear) = case (lhs, rhs) of
        (_, "") -> (Right Nothing, parseYear lhs)
        _       -> (parseMonth lhs, parseYear rhs)
  in case (eMonth, eYear) of
    (Left failure, _)          -> Left $ InvalidOptions failure
    (_, Left failure)          -> Left $ InvalidOptions failure
    (Right mMonth, Right year) -> Right $ Just (DocumentDate mMonth year)

parseMonth :: String -> Either String (Maybe Month)
parseMonth "" = Right Nothing
parseMonth (c:cs) =
  case reads (toUpper c:cs) :: [(Month, String)] of
    [(month, "")] -> Right $ Just month
    _             -> Left $ "Please enter a valid month, without abbreviations"

parseYear :: String -> Either String Integer
parseYear inputYear =
  case reads inputYear :: [(Integer, String)] of
    [(year, "")] -> Right year
    _            -> Left $ "Please enter a valid year"

validateOptionPresence :: Options -> Either EarlyExit Options
validateOptionPresence opts =
  case (title opts, sender opts, recipient opts) of
    (Left failure, _, _) -> Left failure
    (_, Left failure, _) -> Left failure
    (_, _, Left failure) -> Left failure
    _                    -> Right opts
