module Control.Monad.Gratte
  ( Gratte(..)
  , withGratte
  , getOptions, getOption
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

import Control.Monad
import Control.Monad.Trans

import System.Log.GratteLogger

import Gratte.Options

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
  let level = case (verbose opts, silent opts) of
                (True, _) -> Debug
                (_, True) -> Critical
                _         -> Notice
  let settings = LoggerSettings path level
  liftIO $ configureLogger settings
