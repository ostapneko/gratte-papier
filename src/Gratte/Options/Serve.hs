module Gratte.Options.Serve where

import           Options.Applicative

data ServeOptions = ServeOptions
  { port :: Int
  } deriving Show

parseServeOptions :: Parser ServeOptions
parseServeOptions = ServeOptions
                <$> option auto
                  ( long "HTTP port"
                 <> short 'p'
                 <> metavar "PORT"
                 <> value 3000
                 <> help "The HTTP port gratte server will be listening to"
                  )
