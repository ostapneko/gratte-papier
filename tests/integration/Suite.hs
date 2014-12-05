{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Gratte
import Control.Monad.Trans

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

import Network.HTTP
import Network.URI

import Test.Hspec
import TestHelper

import Gratte.Command.Add
import Gratte.Document
import Gratte.Options
import Gratte.Command.Reindex  (reindex)
import Gratte.Command.Search   (getDocs)
import Gratte.Tag
import Gratte.Utils    (getFilesRecurs)

main :: IO ()
main = hspec $ do
  describe "Add and retrieve jpg document" $
    it "Adds a document and allows for its search" $ do
      (copiedDocSize, searchedDoc) <-
        inTestContext $ \ opts tmpDir -> do
          withGratte (opts { optCommand = addCommand }) $ do
            cleanES
            addExampleDoc exampleImageFile
            refreshIndex
          withGratte (opts { optCommand = searchCommand }) $ do
            copiedDocSize' <- getCopiedDocSize (FS.decodeString tmpDir) exampleImageFile
            searchedDoc'   <- getDocs "hspec"
            return (copiedDocSize', searchedDoc')

      assertFileCopy exampleImageFile copiedDocSize
      assertSearchSuccess searchedDoc

  describe "Add and retrieve text PDF document" $
    it "Adds a PDF document as text and allows for its search" $ do
      (copiedDocSize, searchedDoc) <-
        inTestContext $ \ opts tmpDir -> do
          withGratte (opts { optCommand = addCommand }) $ do
            cleanES
            addExampleDoc examplePDFFile
            refreshIndex
          withGratte (opts { optCommand = searchCommand }) $ do
            copiedDocSize' <- getCopiedDocSize (FS.decodeString tmpDir) examplePDFFile
            searchedDoc'   <- getDocs "haskell"
            return (copiedDocSize', searchedDoc')

      assertFileCopy examplePDFFile copiedDocSize
      assertSearchSuccess searchedDoc

  describe "Add and retrieve an image PDF document"
    it "Adds a PDF document as text and allows for its search" $ do
      (copiedDocSize, searchedDoc) <-
        inTestContext $ \ opts tmpDir -> do
          let addOptions' = addOptions { pdfMode = PDFModeImage }
          withGratte (opts { optCommand = AddCmd addOptions' }) $ do
            cleanES
            addExampleDoc examplePDFFile
            refreshIndex
          withGratte (opts { optCommand = searchCommand }) $ do
            copiedDocSize' <- getCopiedDocSize (FS.decodeString tmpDir) examplePDFFile
            searchedDoc'   <- getDocs "haskell"
            return (copiedDocSize', searchedDoc')

      assertFileCopy examplePDFFile copiedDocSize
      assertSearchSuccess searchedDoc

  describe "Reindex" $
    it "Regenerate the ES index" $ do
      searchedDoc <- inTestContext $ \ opts _ -> do
        withGratte (opts { optCommand = addCommand }) $ do
          cleanES
          addExampleDoc exampleImageFile
        withGratte (opts { optCommand = ReindexCmd }) $ do
          reindex
          refreshIndex
        withGratte (opts { optCommand = searchCommand }) (getDocs "hspec")
      assertSearchSuccess searchedDoc


addExampleDoc :: FS.FilePath -> Gratte ()
addExampleDoc file = do
  docs <- createDocuments [file]
  liftIO . putStrLn $ show docs
  archive $ zip [file] docs

refreshIndex :: Gratte ()
refreshIndex = do
  EsHost h <- getOption esHost
  EsIndex i <- getOption esIndex
  let url = h { uriPath = i ++ "/_refresh" }
  _ <- liftIO $ simpleHTTP $ mkRequest POST url :: Request String
  return ()

getCopiedDocSize :: FS.FilePath -> FS.FilePath -> Gratte Integer
getCopiedDocSize tmpDir sourceFile = liftIO $ do
  files <- liftIO $ getFilesRecurs tmpDir
  let copiedFile = head $ filter ((== (FS.extension sourceFile)) . FS.extension) files
  fileSize <- liftIO $ FS.getSize copiedFile
  return fileSize

assertFileCopy :: FS.FilePath -> Integer -> Expectation
assertFileCopy sourceFile actSize = do
  expSize <- liftIO $ FS.getSize sourceFile
  actSize `shouldBe` expSize

assertSearchSuccess :: [Document] -> Expectation
assertSearchSuccess docs = do
  length docs `shouldBe` 1
  docTags (head docs) `shouldBe` [Tag "tag"]
