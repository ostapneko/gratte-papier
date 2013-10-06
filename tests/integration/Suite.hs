import Control.Concurrent
import Control.Monad.Gratte
import Control.Monad.Trans

import System.FilePath
import System.IO

import Network.HTTP

import Test.Hspec
import TestHelper

import Gratte.Add
import Gratte.Document
import Gratte.Options
import Gratte.Reindex  (reindex)
import Gratte.Search   (getDocs)
import Gratte.Tag
import Gratte.Utils    (getFilesRecurs)

main :: IO ()
main = hspec $ do
  describe "Add and retrieve document" $ do
    it "Adds a document and allows for its search" $ do
      (copiedDocSize, searchedDoc) <- do
        inTestContext $ \ opts tmpDir -> do
          withGratte opts $ do
            cleanES
            addExampleDoc
            refreshIndex
            copiedDocSize' <- getCopiedDocSize tmpDir
            searchedDoc'   <- getDocs "hspec"
            return (copiedDocSize', searchedDoc')

      assertFileCopy copiedDocSize
      assertSearchSuccess searchedDoc

  describe "Reindex" $ do
    it "Regenerate the ES index" $ do
      searchedDoc <- inTestContext $ \ opts _ -> do
        withGratte opts $ do
          cleanES
          addExampleDoc
          reindex
          refreshIndex
          getDocs "hspec"
      assertSearchSuccess searchedDoc


addExampleDoc :: Gratte ()
addExampleDoc = do
  docs <- createDocuments [exampleFile]
  liftIO . putStrLn $ show docs
  archive $ zip [exampleFile] docs

refreshIndex :: Gratte ()
refreshIndex = do
  EsHost h <- getOption esHost
  EsIndex i <- getOption esIndex
  let url = h </> i </> "_refresh"
  _ <- liftIO $ simpleHTTP (postRequest url)
  return ()

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
