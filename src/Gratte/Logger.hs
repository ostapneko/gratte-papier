module Gratte.Logger (
  configureLogger
  , logMsg
  ) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Time
import System.IO

import qualified Gratte.Options as Opt

configureLogger :: Opt.Options -> IO ()
configureLogger opts = do
  -- Everything is logged to the file
  logFileHandler <- fileHandler "log.log" DEBUG
  -- Console logging depends on the verbosity in the options
  consoleHandler <- getConsoleHandler opts
  updateGlobalLogger rootLoggerName $
    (setLevel DEBUG . setHandlers [logFileHandler, consoleHandler])

logMsg :: Opt.Options -> Priority -> String -> IO ()
logMsg opts p msg = do
  timeStamp    <- getTimeStamp
  l            <- getRootLogger
  let datedMsg = "[" ++ timeStamp ++ "][" ++
                 show p ++ "] " ++ msg
  logL l p datedMsg

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
                _            -> WARNING
  streamHandler stderr level
