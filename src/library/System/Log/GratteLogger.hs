module System.Log.GratteLogger (
    configureLogger
  , LoggerSettings(..)
  , logMsgIO
  , LogLevel(..)
  ) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler        (setFormatter)
import System.Log.Formatter
import System.IO
import System.Environment
import System.Directory
import System.FilePath

data LogLevel = Debug
              | Info
              | Notice
              | Warning
              | Error
              | Critical
              | Alert
              | Emergency
              deriving (Show, Eq)

logLevelToPriority :: LogLevel -> Priority
logLevelToPriority lvl = case lvl of
  Debug     -> DEBUG
  Info      -> INFO
  Notice    -> NOTICE
  Warning   -> WARNING
  Error     -> ERROR
  Critical  -> CRITICAL
  Alert     -> ALERT
  Emergency -> EMERGENCY

data LoggerSettings = LoggerSettings {
    loggerPath  :: FilePath
  , loggerLevel :: LogLevel
}

logMsgIO :: LogLevel -> String -> IO ()
logMsgIO lvl msg = do
  logger <- getRootLogger
  logL logger (logLevelToPriority lvl) msg

configureLogger :: LoggerSettings -> IO ()
configureLogger (LoggerSettings path level) = do
  prg           <- getProgName
  -- Everything is logged to the file
  createLogDir path
  logFileHandler <- fileHandler path DEBUG
  -- Console logging depends on the verbosity in the options
  consoleHandler <- streamHandler stderr $ logLevelToPriority level
  updateGlobalLogger rootLoggerName
    (setLevel DEBUG
    . setHandlers (
    map (flip setFormatter (getFormatter prg))
    $ [logFileHandler, consoleHandler]))

getFormatter :: String -> LogFormatter (GenericHandler Handle)
getFormatter prg = simpleLogFormatter $ "[$utcTime] [$prio] [" ++ prg ++ "] $msg"

createLogDir :: FilePath -> IO ()
createLogDir path = do
  let folder = takeDirectory path
  createDirectoryIfMissing True folder
