module Gratte.Options
  ( Options(..)
  , AddOptions(..)
  , SearchOptions(..)
  , defaultEsHost
  , EsHost(..)
  , EsIndex(..)
  , PDFMode(..)
  , OutputFormat(..)
  , Verbosity(..)
  , Command(..)
  , parseOptions
  ) where

import           Control.Monad

import           Data.Char
import qualified Data.List.Split as SPL

import qualified Filesystem.Path.CurrentOS as FS

import           Network.URI

import           Options.Applicative

import           Gratte.Tag
import           Gratte.Document

newtype EsHost   = EsHost URI     deriving Show
newtype EsIndex  = EsIndex String deriving Show

data PDFMode = PDFModeImage
             | PDFModeText
             deriving Show

data OutputFormat = OutputFormatCompact
                  | OutputFormatDetailed
                  deriving Show

data Verbosity = VerbositySilent
               | VerbosityNormal
               | VerbosityVerbose
               deriving Show

data Command = AddCmd     AddOptions
             | ReindexCmd
             | SearchCmd  SearchOptions
             deriving Show

data AddOptions = AddOptions
  { pdfMode   :: PDFMode
  , ocr       :: Bool
  , title     :: DocumentTitle
  , sender    :: DocumentSender
  , recipient :: DocumentRecipient
  , date      :: DocumentDate
  , tags      :: [Tag]
  , newFiles  :: [FS.FilePath]
  } deriving Show

data SearchOptions = SearchOptions
  { outputFormat :: OutputFormat
  , resultSize   :: Int
  , query        :: String
  } deriving Show

data Options = Options
  { verbosity   :: Verbosity
  , esHost      :: EsHost
  , esIndex     :: EsIndex
  , folder      :: FS.FilePath
  , logFilePath :: FS.FilePath
  , optCommand  :: Command
  } deriving Show

defaultEsHost :: EsHost
defaultEsHost = EsHost $ URI
  { uriScheme = "http:"
  , uriAuthority = Just $ URIAuth "" "localhost" ":9200"
  , uriPath = ""
  , uriQuery = ""
  , uriFragment = ""
  }

defaultOptions = Options
  { verbosity = VerbosityNormal
  , esHost = defaultEsHost
  , esIndex = EsIndex "gratte"
  , folder = FS.decodeString "/var/gratte"
  , logFilePath = FS.decodeString "/var/log/gratte/gratte.log"
  , optCommand = error "Command not set in the options"
  }

parseOptions :: Parser Options
parseOptions = Options
           <$> nullOption
               ( long "verbosity"
              <> short 'v'
              <> metavar "VERBOSITY"
              <> value (verbosity defaultOptions)
              <> help "the output verbosity from 0 (silent) to 2 (verbose)"
              <> reader parseVerbosity )
           <*> nullOption
               ( long "es-host"
              <> metavar "HOST"
              <> value (esHost defaultOptions)
              <> help "The ElasticSearch server hostname"
              <> reader parseEsHost )
           <*> nullOption
               ( long "es-index"
              <> metavar "NAME"
              <> value (esIndex defaultOptions)
              <> help "The index for the documents in ElasticSearch"
              <> reader parseEsIndex )
           <*> nullOption
               ( long "folder"
              <> metavar "PATH"
              <> value (folder defaultOptions)
              <> help "The directory used to store the documents and their metadata"
              <> reader parsePath )
           <*> nullOption
               ( long "log-file"
              <> metavar "PATH"
              <> value (logFilePath defaultOptions)
              <> help "The log file"
              <> reader parsePath )
           <*> subparser
                 ( command "add" addParserInfo
                <> command "search" searchParserInfo
                <> command "reindex" reindexParserInfo)


searchParserInfo :: ParserInfo Command
searchParserInfo = info (helper <*> (SearchCmd <$> parseSearchOptions)) fullDesc

parseSearchOptions :: Parser SearchOptions
parseSearchOptions = SearchOptions
                 <$> flag OutputFormatDetailed OutputFormatCompact
                     ( long "compact"
                    <> short 'c'
                    <> help "Just output file names" )
                 <*> option
                     ( long "result-size"
                    <> short 'n'
                    <> metavar "N"
                    <> value 20
                    <> help "The number of returned results" )
                 <*> argument str
                     ( help "Search query"
                    <> metavar "QUERY" )

addParserInfo :: ParserInfo Command
addParserInfo = info (helper <*> (AddCmd <$> parseAddOptions)) fullDesc

parseAddOptions :: Parser AddOptions
parseAddOptions = AddOptions
              <$> flag PDFModeImage PDFModeText
                  ( long "text-pdf"
                 <> help "Use this switch if the document to add is a PDF and not an image (copy-pastable text)" )
              <*> flag False True
                  ( long "ocr"
                 <> short 'o'
                 <> help "Uses OCR to try extract the text from the documents and add it as searchable metadata. Requires tesseract to be installed." )
              <*> nullOption
                  ( long "title"
                 <> short 't'
                 <> metavar "TITLE"
                 <> help "The title of the documents. If more than one documents are present, add a page number after it (e.g. \"My doc (Page 1)\", etc. )"
                 <> reader parseTitle )
              <*> nullOption
                  ( long "sender"
                 <> short 's'
                 <> metavar "NAME"
                 <> help "The document's sender"
                 <> reader parseSender )
              <*> nullOption
                  ( long "recipient"
                 <> short 'r'
                 <> metavar "NAME"
                 <> help "The document's recipient"
                 <> reader parseRecipient )
              <*> nullOption
                  ( long "date"
                 <> short 'd'
                 <> metavar "\"MONTH YEAR\""
                 <> help "The documents' month (optionaly) and year. If provided, the date MUST be in the form \"September 2013\"."
                 <> reader parseDate )
              <*> nullOption
                  ( long "tags"
                 <> short 'T'
                 <> metavar "TAG1,TAG2"
                 <> value []
                 <> help "Add a comma or colon separated list of tags to the document"
                 <> reader parseTags )
              <*> arguments (\ s -> FS.decodeString `liftM` str s)
                 ( help "Files to add"
                <> metavar "FILES" )

reindexParserInfo :: ParserInfo Command
reindexParserInfo = info (helper <*> pure ReindexCmd) fullDesc

parseVerbosity :: Monad m => String -> m Verbosity
parseVerbosity "0" = return VerbositySilent
parseVerbosity "1" = return VerbosityNormal
parseVerbosity "2" = return VerbosityVerbose
parseVerbosity _   = fail "The verbosity should be 0, 1 or 2"

parseEsHost :: Monad m => String -> m EsHost
parseEsHost input =
  case parseAbsoluteURI input of
    Just uri -> return $ EsHost uri
    _        -> fail "The EsHost must be a valid absolute URI"

parseEsIndex :: Monad m => String -> m EsIndex
parseEsIndex = return . EsIndex

parsePath :: Monad m => String -> m FS.FilePath
parsePath = return . FS.decodeString

parseTitle :: Monad m => String -> m DocumentTitle
parseTitle = return . DocumentTitle

parseSender :: Monad m => String -> m DocumentSender
parseSender = return . DocumentSender

parseRecipient :: Monad m => String -> m DocumentRecipient
parseRecipient = return . DocumentRecipient

parseDate :: Monad m => String -> m DocumentDate
parseDate arg =
  let (lhs, rhs) = (\ (x, y) -> (x, drop 1 y)) . break (==' ') $ arg
      (eMonth, eYear) = case (lhs, rhs) of
        (_, "") -> (Right Nothing, parseYear lhs)
        _       -> (parseMonth lhs, parseYear rhs)
  in case (eMonth, eYear) of
    (Left failure, _)          -> fail failure
    (_, Left failure)          -> fail failure
    (Right mMonth, Right year) -> return $ DocumentDate mMonth year

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

parseTags :: Monad m => String -> m [Tag]
parseTags = return . map (Tag . dropWhile (==' ')) . SPL.splitOneOf ",:"
