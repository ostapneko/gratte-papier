module TestHelper
  ( inTestContext
  , cleanES
  ) where

import Control.Monad.Gratte
import Control.Monad.Trans

import System.FilePath
import System.IO.Temp

import Network.HTTP
import Network.URI

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
testOpts tmpDir = Options {
    verbose      = False
  , silent       = True
  , esHost       = EsHost "http://localhost:9200"
  , title        = DocumentTitle "doc"
  , folder       = GratteFolder tmpDir
  , dryRun       = False
  , ocr          = True
  , logFilePath  = tmpDir </> "log"
  , outputFormat = DetailedFormat
  , esIndex      = EsIndex "gratte_test"
  , pdfMode      = NoPDFMode
  , resultSize   = 100
  , tags         = [Tag "tag"]
}

cleanES :: Gratte ()
cleanES = do
  (EsHost esHost')   <- getOption esHost
  (EsIndex esIndex') <- getOption esIndex
  let url = esHost' </> esIndex'
  let mURI = parseURI url
  case mURI of
    Nothing -> do
      logCritical $ "Malformed URL: " ++ url
    Just uri -> do
      let request = mkRequest DELETE uri :: Request String
      res <- liftIO . simpleHTTP $ request
      case res of
        Left _  -> logCritical "Error trying to delete the index"
        Right _ -> return ()
