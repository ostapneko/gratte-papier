{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Gratte
import Control.Monad.Trans

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

import Network.HTTP
import Network.URI

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
          withGratte (opts { optCommand = addCommand }) $ do
            cleanES
            addExampleDoc
            refreshIndex
          withGratte (opts { optCommand = searchCommand }) $ do
            copiedDocSize' <- getCopiedDocSize $ FS.decodeString tmpDir
            searchedDoc'   <- getDocs "hspec"
            return (copiedDocSize', searchedDoc')

      assertFileCopy copiedDocSize
      assertSearchSuccess searchedDoc

  describe "Reindex" $ do
    it "Regenerate the ES index" $ do
      searchedDoc <- inTestContext $ \ opts _ -> do
        withGratte (opts { optCommand = addCommand }) $ do
          cleanES
          addExampleDoc
        withGratte (opts { optCommand = ReindexCmd }) $ do
          reindex
          refreshIndex
        withGratte (opts { optCommand = searchCommand }) (getDocs "hspec")
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
  let url = h { uriPath = i ++ "/_refresh" }
  _ <- liftIO $ simpleHTTP $ (mkRequest POST url :: Request String)
  return ()

getCopiedDocSize :: FS.FilePath -> Gratte Integer
getCopiedDocSize tmpDir = liftIO $ do
  files <- getFilesRecurs tmpDir
  let file = head $ filter ((== Just "png") . FS.extension) files
  fileSize <- liftIO $ FS.getSize file
  return fileSize

assertFileCopy :: Integer -> Expectation
assertFileCopy actSize = do
  expSize <- liftIO $ FS.getSize exampleFile
  actSize `shouldBe` expSize

assertSearchSuccess :: [Document] -> Expectation
assertSearchSuccess docs = do
  length docs `shouldBe` 1
  docTags (head docs) `shouldBe` [Tag "tag"]
