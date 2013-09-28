import Control.Concurrent
import Control.Monad.Gratte
import Control.Monad.Trans

import System.FilePath
import System.IO

import Test.Hspec
import TestHelper

import Gratte.Document
import Gratte.Add
import Gratte.Search   (getDocs)
import Gratte.Utils    (getFilesRecurs)
import Gratte.Tag

main :: IO ()
main = hspec $ do
  describe "Add and retrieve document" $ do
    it "Adds a document and allows for its search" $ do
      (copiedDocSize, searchedDoc) <- do
        inTestContext $ \ opts tmpDir -> do
          withGratte opts $ do
            cleanES
            addExampleDoc
            waitForIndexing
            copiedDocSize' <- getCopiedDocSize tmpDir
            searchedDoc'   <- getDocs "hspec"
            return (copiedDocSize', searchedDoc')

      assertFileCopy copiedDocSize
      assertSearchSuccess searchedDoc


addExampleDoc :: Gratte ()
addExampleDoc = do
  docs <- createDocuments [exampleFile]
  archive $ zip [exampleFile] docs

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
  docTags (head docs) `shouldBe` [Tag "tag"]

getFileSize :: FilePath -> IO Integer
getFileSize file = withFile file ReadMode hFileSize

exampleFile :: FilePath
exampleFile = "tests" </> "integration" </> "resources" </> "example.png"
