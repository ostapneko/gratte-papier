module Gratte.Reindex (
  reindex
  , getHash
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import           Data.Maybe
import qualified Data.Text as T
import           Data.Char

import System.FilePath
import System.Directory

import Network.HTTP
import Network.URI

import qualified Gratte.Options  as O
import qualified Gratte.TypeDefs as G
import           Gratte.Add      (extractText, sendToES)
import           Gratte.Utils    (getFilesRecurs)

reindex :: Gratte ()
reindex = do
  deleteIndex
  importFiles

deleteIndex :: Gratte ()
deleteIndex = do
  G.EsHost esHost <- getOption O.esHost
  let url = esHost ++ "/gratte/document/"
  let uri = fromJust $ parseURI url
  result <- liftIO . simpleHTTP $ mkRequest DELETE uri
  case result of
    Right (Response (2, _, _) _ _ body) -> logNotice $ "Index deleted: " ++ body
    _                                   -> logError "Something went wrong in the index deletion. Is it already deleted?"

importFiles :: Gratte ()
importFiles = do
  folder <- getOption O.folder
  logNotice $ "Starting file imports from folder: " ++ folder ++ " ..."
  files <- liftIO $ (filter notTagFile) `liftM` getFilesRecurs folder
  mapM_ importFile files
  logNotice $ show (length files) ++ " files imported successfully."

notTagFile :: FilePath -> Bool
notTagFile file = takeBaseName file /= "tags"

importFile :: FilePath -> Gratte ()
importFile file = do
  logDebug $ "Importing File " ++ file
  doc <- createDoc file
  sendToES doc

createDoc :: FilePath -> Gratte G.Document
createDoc file = do
  let hash = getHash file
  tags     <- liftIO . getTags $ file
  useOcr   <- getOption O.ocr
  freeText <- case useOcr of
                  True  -> liftIO . extractText $ file
                  False -> return T.empty
  return $ G.Document {
      G.hash      = G.Hash hash
    , G.filepath  = file
    , G.tags      = tags
    , G.freeText  = freeText
    }

-- foo/bar/a/2/3/4/doc-5678 -> a2345678
getHash :: FilePath -> String
getHash fp =
  let (fileName:dirs) = take 4 . reverse . splitDirectories . dropExtension $ fp
      fileNameNoPrefix = reverse . takeWhile isAlphaNum . reverse $ fileName
  in  concat $ reverse dirs ++ [fileNameNoPrefix]

getTags :: FilePath -> IO [G.Tag]
getTags file = do
  let tagPath = takeDirectory file ++ "/tags"
  tagFileExists <- doesFileExist tagPath
  case tagFileExists of
    True -> do
      tagFileContent <- readFile tagPath
      return $ map G.Tag (lines tagFileContent)
    False -> return []
