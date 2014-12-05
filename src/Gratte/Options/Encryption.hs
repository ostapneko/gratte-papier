module Gratte.Options.Encryption where

import           Options.Applicative
import qualified Filesystem.Path.CurrentOS as FS

data EncryptionOptions = EncryptionOptions
  { encryptFolder :: FS.FilePath
  , password      :: Maybe String
  } deriving Show

parseEncryptionOptions :: Parser EncryptionOptions
parseEncryptionOptions = EncryptionOptions
                     <$> option (FS.decodeString <$> str)
                       ( long "encdir"
                      <> short 'e'
                      <> metavar "DIR"
                      <> help "The folder where the encrypted file are")
                     <*> option auto
                       ( long "password"
                      <> short 'p'
                      <> metavar "PASSWORD"
                      <> help "Encryption password. It's recommended to pass the GRATTE_PASSWORD env var instead.")
