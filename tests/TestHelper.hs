{-# LANGUAGE OverloadedStrings #-}

module TestHelper
  ( inTestContext
  , cleanES
  , addCommand
  , searchCommand
  , exampleFile
  ) where

import Control.Monad.Gratte
import Control.Monad.Trans

import Data.Monoid

import System.IO.Temp

import qualified Filesystem.Path.CurrentOS as FS

import Network.HTTP
import Network.URI hiding (query)

import Gratte.Document
import Gratte.Options
import Gratte.Tag

-- | Execute the test in a test context, using 'testOpts'
inTestContext :: (Options -> FilePath -> IO a) -- ^ A function taking as argmument the test options and the temp folder
              -> IO a
inTestContext f = do
  withSystemTempDirectory "gratte-test" $ \ tmpDir -> do
    let opts = testOpts tmpDir
    f opts tmpDir

-- | Options preset for test, using a test folder meant
-- to be cleaned up, and an ES test index
testOpts :: FilePath -- ^ The temp folder to use for tests
         -> Options
testOpts tmpDir = Options
  { verbosity   = VerbositySilent
  , esHost      = defaultEsHost
  , esIndex     = EsIndex "gratte_test"
  , folder      = FS.decodeString tmpDir
  , logFilePath = FS.decodeString tmpDir <> "temp.log"
  , optCommand  = error "Command not set"
  }

exampleFile :: FS.FilePath
exampleFile = mconcat ["tests", "integration", "resources", "example.png"]

addCommand :: Command
addCommand = AddCmd $ AddOptions
  { pdfMode   = PDFModeText
  , ocr       = True
  , title     = DocumentTitle "title"
  , sender    = DocumentSender "sender"
  , recipient = DocumentRecipient "recipient"
  , date      = DocumentDate Nothing 2013
  , tags      = [Tag "tag"]
  , newFiles  = [exampleFile]
  }

searchCommand :: Command
searchCommand = SearchCmd $ SearchOptions
  { outputFormat = OutputFormatCompact
  , resultSize   = 20
  , query        = "hspec"
  }

cleanES :: Gratte ()
cleanES = do
  EsHost esHost'   <- getOption esHost
  EsIndex esIndex' <- getOption esIndex
  let url = esHost' { uriPath = esIndex' }
  let request = mkRequest DELETE url :: Request String
  res <- liftIO . simpleHTTP $ request
  case res of
    Left _  -> logCritical "Error trying to delete the index"
    Right _ -> return ()
