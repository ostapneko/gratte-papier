import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans

import           Data.Char

import qualified Filesystem.Path.CurrentOS as FS

import Options.Applicative

import Gratte.Options
import Gratte.Document

import Gratte.Command.Add
import Gratte.Command.Search
import Gratte.Command.Reindex
import Gratte.Command.Serve
import Gratte.Command.Encryption

main :: IO ()
main = do
  let optDescrs = info (helper <*> parseOptions) fullDesc
  opts <- execParser optDescrs
  withGratte opts setupLogger
  case optCommand opts of
    ServeCmd serveOpts   -> serve opts serveOpts
    AddCmd addOpts       -> withGratte opts $ addFiles (newFiles addOpts)
    ReindexCmd           -> withGratte opts reindex
    SearchCmd searchOpts -> withGratte opts $ searchDocs (query searchOpts)
    Encrypt encOpts      -> withGratte opts $ encryptFiles encOpts
    Decrypt encOpts      -> withGratte opts $ decryptFiles encOpts

askForConfirmation :: Bool    -- ^ Is it the first time we ask the question ?
                   -> IO Bool
askForConfirmation isFirstTime = do
  let msg = if isFirstTime
              then "Do you want to procede? [y/N]"
              else "Please type \"y\" or \"n\""
  putStrLn msg
  answer <- getLine
  case map toLower answer of
    "y" -> return True
    "n" -> return False
    _   -> askForConfirmation False

addFiles :: [FS.FilePath] -> Gratte ()
addFiles files = do
  documents <- createDocuments files
  docFolder <- getOption folder
  liftIO . putStrLn $ createReport docFolder documents
  confirmation <- liftIO $ askForConfirmation True
  when confirmation (archive (zip files documents))
