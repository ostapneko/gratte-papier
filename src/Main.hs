import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans

import           Data.Char

import qualified Filesystem.Path.CurrentOS as FS

import           Options.Applicative

import           Gratte.Options
import           Gratte.Document
import qualified Gratte.Add             as Add
import qualified Gratte.Search          as Search
import qualified Gratte.Reindex         as Reindex

main :: IO ()
main = do
  let optDescrs = info (helper <*> parseOptions) fullDesc
  opts <- execParser optDescrs
  withGratte opts $ do
    setupLogger
    case optCommand opts of
      AddCmd addOpts       -> addFiles $ newFiles addOpts
      ReindexCmd           -> Reindex.reindex
      SearchCmd searchOpts -> Search.searchDocs $ query searchOpts

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
  documents <- Add.createDocuments files
  docFolder <- getOption folder
  liftIO . putStrLn $ createReport docFolder documents
  confirmation <- liftIO $ askForConfirmation True
  when confirmation (Add.archive (zip files documents))
