module Gratte.Options.Search where

import           Options.Applicative

data OutputFormat = OutputFormatCompact
                  | OutputFormatDetailed
                  deriving Show

data SearchOptions = SearchOptions
  { outputFormat :: OutputFormat
  , resultSize   :: Int
  , query        :: String
  } deriving Show

defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
  { outputFormat = OutputFormatDetailed
  , resultSize = 20
  , query = ""
  }

parseSearchOptions :: Parser SearchOptions
parseSearchOptions = SearchOptions
                 <$> flag OutputFormatDetailed OutputFormatCompact
                     ( long "compact"
                    <> short 'c'
                    <> help "Just output file names" )
                 <*> option auto
                     ( long "result-size"
                    <> short 'n'
                    <> metavar "N"
                    <> value (resultSize defaultSearchOptions)
                    <> help "The number of returned results" )
                 <*> argument str
                     ( help "Search query"
                    <> metavar "QUERY" )
