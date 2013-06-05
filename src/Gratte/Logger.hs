module Gratte.Logger (
  configureLogger
  , logMsg
  , Priority(..)
  ) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Time
import System.IO

import qualified Gratte.Options as Opt

configureLogger :: Opt.Options -> IO ()
configureLogger opts = do
  -- Everything is logged to the file
  logFileHandler <- fileHandler "/var/log/gratte/gratte.log" DEBUG
  -- Console logging depends on the verbosity in the options
  consoleHandler <- getConsoleHandler opts
  updateGlobalLogger rootLoggerName $
    (setLevel DEBUG . setHandlers [logFileHandler, consoleHandler])

logMsg :: Priority -> String -> IO ()
logMsg level msg = do
  timeStamp    <- getTimeStamp
  logger       <- getRootLogger
  let datedMsg = "[" ++ timeStamp  ++ "]" ++
                 "[" ++ show level ++ "]" ++
                 " " ++ msg
  logL logger level datedMsg

getTimeStamp :: IO String
getTimeStamp = do
  time     <- getClockTime
  dateTime <- toCalendarTime time
  return $ calendarTimeToString dateTime

getConsoleHandler :: Opt.Options -> IO (GenericHandler Handle)
getConsoleHandler opts = do
  let level = case (Opt.verbose opts, Opt.silent opts) of
                (True, _)    -> DEBUG
                (_   , True) -> CRITICAL
                _            -> NOTICE
  streamHandler stderr level
