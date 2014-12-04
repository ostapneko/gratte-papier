module Control.Monad.Gratte
  ( Gratte(..)
  , withGratte
  , getOptions, getOption
  , getAddOptions, getAddOption
  , getSearchOptions, getSearchOption
  , logMsg
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logAlert
  , logEmergency
  , setupLogger
  ) where

import           Control.Monad
import           Control.Monad.Trans

import           System.Log.GratteLogger

import           Gratte.Options

data Gratte a = Gratte { gratte :: Options -> IO a }

withGratte :: Options -> Gratte a -> IO a
withGratte = flip gratte

instance Monad Gratte where
  return x = Gratte $ \_ -> return x
  discovery >>= f = Gratte $ \opts -> do
    x <- gratte discovery opts
    gratte (f x) opts
  fail = Gratte . fail

instance MonadIO Gratte where
  liftIO = Gratte . const

getOptions :: Gratte Options
getOptions = Gratte return

getOption :: (Options -> a) -> Gratte a
getOption getter = getter `liftM` getOptions

getAddOptions :: Gratte AddOptions
getAddOptions = do
  cmd <- getOption optCommand
  case cmd of
    AddCmd addOpts -> return addOpts
    _              -> error $ "Trying to access add options while given the command: " ++ show cmd

getAddOption :: (AddOptions -> a) -> Gratte a
getAddOption getter = getter `liftM` getAddOptions

getSearchOptions :: Gratte SearchOptions
getSearchOptions = do
  cmd <- getOption optCommand
  case cmd of
    SearchCmd searchOpts -> return searchOpts
    _                    -> error $ "Trying to access search options while given the command: " ++ show cmd

getSearchOption :: (SearchOptions -> a) -> Gratte a
getSearchOption getter = getter `liftM` getSearchOptions

logMsg :: LogLevel -> String -> Gratte ()
logMsg lvl msg = liftIO $ logMsgIO lvl msg

logDebug :: String -> Gratte ()
logDebug = logMsg Debug

logInfo :: String -> Gratte ()
logInfo = logMsg Info

logNotice :: String -> Gratte ()
logNotice = logMsg Notice

logWarning :: String -> Gratte ()
logWarning = logMsg Warning

logError :: String -> Gratte ()
logError = logMsg Error

logCritical :: String -> Gratte ()
logCritical = logMsg Critical

logAlert :: String -> Gratte ()
logAlert = logMsg Alert

logEmergency :: String -> Gratte ()
logEmergency = logMsg Emergency

setupLogger :: Gratte ()
setupLogger = do
  opts <- getOptions
  let path = logFilePath opts
  let level = case verbosity opts of
                VerbositySilent  -> Critical
                VerbosityNormal  -> Notice
                VerbosityVerbose -> Debug
  let settings = LoggerSettings path level
  liftIO $ configureLogger settings
