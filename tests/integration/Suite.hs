import Control.Monad.Trans
import Control.Monad.Gratte
import Control.Concurrent

import System.IO
import System.IO.Temp
import System.FilePath

import Network.HTTP
import Network.URI

import Test.Hspec

import Gratte.Options
import Gratte.TypeDefs
import Gratte.Add    (addDocuments)
import Gratte.Search (getDocs)
import Gratte.Utils  (getFilesRecurs)

main :: IO ()
main = hspec $ do
  describe "Add and retrieve document" $ do
    it "Adds a document and allows for its search" $ do
      (copiedDocSize, searchedDoc) <- do
        liftIO $ withSystemTempDirectory "gratte-test" $ \tmpDir -> do
          flip gratte (opts tmpDir) $ do
            cleanES
            addExampleDoc
            waitForIndexing
            copiedDocSize' <- getCopiedDocSize tmpDir
            searchedDoc'   <- getDocs "hspec"
            return (copiedDocSize', searchedDoc')

      assertFileCopy copiedDocSize
      assertSearchSuccess searchedDoc

opts :: FilePath -> Options
opts tmpDir = Options {
    verbose      = False
  , silent       = True
  , mode         = AddMode
  , esHost       = EsHost "http://localhost:9200"
  , prefix       = Prefix "doc"
  , folder       = tmpDir
  , dryRun       = False
  , ocr          = True
  , logFilePath  = tmpDir </> "log"
  , outputFormat = DetailedFormat
  , esIndex      = EsIndex "gratte_test"
}


addExampleDoc :: Gratte ()
addExampleDoc = do
  addDocuments [Tag "tag"] [exampleFile]

waitForIndexing :: Gratte ()
waitForIndexing = liftIO $ threadDelay 2000000

getCopiedDocSize :: FilePath -> Gratte Integer
getCopiedDocSize tmpDir = liftIO $ do
  files <- getFilesRecurs tmpDir
  let file = head $ filter ((== ".png") . takeExtensions) files
  fileSize <- liftIO $ getFileSize file
  return fileSize

assertFileCopy :: Integer -> Expectation
assertFileCopy actSize = do
  expSize <- liftIO $ getFileSize exampleFile
  actSize `shouldBe` expSize

assertSearchSuccess :: [Document] -> Expectation
assertSearchSuccess docs = do
  length docs `shouldBe` 1
  tags (head docs) `shouldBe` [Tag "tag"]

cleanES :: Gratte ()
cleanES = do
  (EsHost esHost)   <- getOption esHost
  (EsIndex esIndex) <- getOption esIndex
  let url = esHost </> esIndex
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

getFileSize :: FilePath -> IO Integer
getFileSize file = withFile file ReadMode hFileSize

exampleFile :: FilePath
exampleFile = "tests" </> "integration" </> "resources" </> "example.png"
