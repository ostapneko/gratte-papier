import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           Data.Char
import qualified Data.List             as L

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Gratte.Options
import           Gratte.Document
import qualified Gratte.Add             as Add
import qualified Gratte.Search          as Search
import qualified Gratte.Reindex         as Reindex

main :: IO ()
main = do
  args <- getArgs
  let (actions, params, errors) = getOpt Permute optionDescrs args
  defaultOptions' <- defaultOptions
  eOpts <- runEitherT $  L.foldl' (>>=) (right defaultOptions') actions
  case eOpts of
    Right opts -> case (validateOptionPresence opts) of
      Left (InvalidOptions msg) -> do
        hPutStrLn stderr $ "Error while trying to validate the document: " ++ msg
        exitFailure
      _ -> withGratte opts $ do
        setupLogger
        case (params, errors) of
          ("add":files, []) -> do
            documents <- Add.createDocuments files
            liftIO . putStrLn $ createReport documents
            confirmation <- liftIO $ askForConfirmation True
            when (confirmation) (Add.archive (zip files documents))
          (["reindex"], []) -> Reindex.reindex
          ([]         , []) -> liftIO usage
          (files      , []) -> Search.searchDocs $ unwords files
          _                 -> mapM_ logCritical errors
    Left UsageWithSuccess -> usage >> exitSuccess
    Left (InvalidOptions msg) -> do
      hPutStrLn stderr $ "Error while parsing the options: " ++ msg
      usage
      exitFailure

askForConfirmation :: Bool    -- ^ Is it the first time we ask the question ?
                   -> IO Bool
askForConfirmation isFirstTime = do
  let msg = if (isFirstTime)
              then "Do you want to procede? [y/N]"
              else "Please type \"y\" or \"n\""
  putStrLn msg
  answer <- getLine
  case map toLower answer of
    "y" -> return True
    "n" -> return False
    _   -> askForConfirmation False
