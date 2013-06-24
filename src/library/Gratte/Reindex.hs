module Gratte.Reindex (
  reindex
  , getHash
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import Data.Maybe
import Data.Char

import System.FilePath
import System.Directory

import Network.HTTP
import Network.URI

import Gratte.Options
import Gratte.Document
import Gratte.Add           (sendToES)
import Gratte.TextExtractor (extractText)
import Gratte.Utils         (getFilesRecurs)

reindex :: Gratte ()
reindex = do
  deleteIndex
  importFiles

deleteIndex :: Gratte ()
deleteIndex = do
  EsHost h <- getOption esHost
  let url = h </> "gratte" </> "document"
  let uri = fromJust $ parseURI url
  result <- liftIO . simpleHTTP $ mkRequest DELETE uri
  case result of
    Right (Response (2, _, _) _ _ body) -> logNotice $ "Index deleted: " ++ body
    _                                   -> logError "Something went wrong in the index deletion. Is it already deleted?"

importFiles :: Gratte ()
importFiles = do
  f <- getOption folder
  logNotice $ "Starting file imports from folder: " ++ f ++ " ..."
  files <- liftIO $ (filter notTagFile) `liftM` getFilesRecurs f
  mapM_ importFile files
  logNotice $ show (length files) ++ " files imported successfully."

notTagFile :: FilePath -> Bool
notTagFile file = takeBaseName file /= "tags"

importFile :: FilePath -> Gratte ()
importFile file = do
  logDebug $ "Importing File " ++ file
  doc <- createDoc file
  sendToES doc

createDoc :: FilePath -> Gratte Document
createDoc file = do
  let h = getHash file
  ts     <- liftIO . getTags $ file
  ft     <- extractText file
  return $ Document {
      hash      = DocumentHash h
    , filepath  = file
    , tags      = ts
    , freeText  = ft
    }

-- foo/bar/a/2/3/4/doc-5678 -> a2345678
getHash :: FilePath -> String
getHash fp =
  let (fileName:dirs) = take 4 . reverse . splitDirectories . dropExtension $ fp
      fileNameNoPrefix = reverse . takeWhile isAlphaNum . reverse $ fileName
  in  concat $ reverse dirs ++ [fileNameNoPrefix]

getTags :: FilePath -> IO [Tag]
getTags file = do
  let tagPath = takeDirectory file ++ "/tags"
  tagFileExists <- doesFileExist tagPath
  case tagFileExists of
    True -> do
      tagFileContent <- readFile tagPath
      return $ map Tag (lines tagFileContent)
    False -> return []
