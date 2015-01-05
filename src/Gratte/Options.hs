module Gratte.Options
  ( Options(..)
  , AddOptions(..)
  , SearchOptions(..)
  , ServeOptions(..)
  , defaultEsHost
  , defaultOptions
  , defaultSearchOptions
  , EsHost(..)
  , EsIndex(..)
  , PDFMode(..)
  , OutputFormat(..)
  , Verbosity(..)
  , Command(..)
  , parseOptions
  ) where

import qualified Filesystem.Path.CurrentOS as FS

import           Network.URI hiding (query)

import           Options.Applicative

import           Gratte.Options.Add
import           Gratte.Options.Search
import           Gratte.Options.Serve
import           Gratte.Options.Encryption

newtype EsHost   = EsHost URI     deriving Show
newtype EsIndex  = EsIndex String deriving Show

data Verbosity = VerbositySilent
               | VerbosityNormal
               | VerbosityVerbose
               deriving Show

data Command = AddCmd      AddOptions
             | ReindexCmd
             | SearchCmd   SearchOptions
             | ServeCmd    ServeOptions
             | Encrypt     EncryptionOptions
             | Decrypt     EncryptionOptions
             deriving Show

data Options = Options
  { verbosity    :: Verbosity
  , esHost       :: EsHost
  , esIndex      :: EsIndex
  , folder       :: FS.FilePath
  , logFilePath  :: FS.FilePath
  , passwordFile :: Maybe FS.FilePath
  , optCommand   :: Command
  } deriving Show

defaultEsHost :: EsHost
defaultEsHost = EsHost URI
  { uriScheme    = "http:"
  , uriAuthority = Just $ URIAuth "" "localhost" ":9200"
  , uriPath      = ""
  , uriQuery     = ""
  , uriFragment  = ""
  }

defaultOptions :: Options
defaultOptions = Options
  { verbosity    = VerbosityNormal
  , esHost       = defaultEsHost
  , esIndex      = EsIndex "gratte"
  , folder       = FS.decodeString "/var/gratte"
  , logFilePath  = FS.decodeString "/var/log/gratte/gratte.log"
  , passwordFile = Nothing
  , optCommand   = error "Command not set in the options"
  }

parseOptions :: Parser Options
parseOptions = Options
           <$> option (str >>= parseVerbosity)
               ( long "verbosity"
              <> short 'v'
              <> metavar "VERBOSITY"
              <> value (verbosity defaultOptions)
              <> help "the output verbosity from 0 (silent) to 2 (verbose)")
           <*> option (str >>= parseEsHost)
               ( long "es-host"
              <> metavar "HOST"
              <> value (esHost defaultOptions)
              <> help "The ElasticSearch server hostname")
           <*> option (EsIndex <$> str)
               ( long "es-index"
              <> metavar "NAME"
              <> value (esIndex defaultOptions)
              <> help "The index for the documents in ElasticSearch")
           <*> option (FS.decodeString <$> str)
               ( long "folder"
              <> metavar "PATH"
              <> value (folder defaultOptions)
              <> help "The directory used to store the documents and their metadata")
           <*> option (FS.decodeString <$> str)
               ( long "log-file"
              <> metavar "PATH"
              <> value (logFilePath defaultOptions)
              <> help "The log file")
           <*> option (Just . FS.decodeString <$> str)
               ( long "password-file"
              <> short 'p'
              <> metavar "PATH"
              <> value Nothing
              <> help "The password file for encrypting and decrypting")
           <*> subparser
                 ( command "add"     addParserInfo
                <> command "search"  searchParserInfo
                <> command "reindex" reindexParserInfo
                <> command "serve"   serveParserInfo
                <> command "encrypt" encryptParserInfo
                <> command "decrypt" decryptParserInfo)

parserInfo :: Parser Command -> ParserInfo Command
parserInfo cmd = info (helper <*> cmd) fullDesc

addParserInfo :: ParserInfo Command
addParserInfo = parserInfo $ AddCmd <$> parseAddOptions

searchParserInfo :: ParserInfo Command
searchParserInfo = parserInfo $ SearchCmd <$> parseSearchOptions

reindexParserInfo :: ParserInfo Command
reindexParserInfo = parserInfo $ pure ReindexCmd

serveParserInfo :: ParserInfo Command
serveParserInfo = parserInfo $ ServeCmd <$> parseServeOptions

encryptParserInfo :: ParserInfo Command
encryptParserInfo = parserInfo $ Encrypt <$> parseEncryptionOptions

decryptParserInfo :: ParserInfo Command
decryptParserInfo = parserInfo $ Decrypt <$> parseEncryptionOptions

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
