module Gratte.Reindex (
  reindex
  , getHash
  ) where

import Control.Monad

import           Data.Maybe
import qualified Data.Text as T
import           Data.Char

import System.FilePath
import System.Directory

import Network.HTTP
import Network.URI

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import           Gratte.Add      (extractText, sendToES)
import           Gratte.Logger
import           Gratte.Utils    (getFilesRecurs)

reindex :: Opt.Options -> IO ()
reindex opts = do
  deleteIndex opts
  importFiles opts

deleteIndex :: Opt.Options -> IO ()
deleteIndex opts = do
  let (G.EsHost esHost) = Opt.esHost opts
  let url = esHost ++ "/gratte/document/"
  let uri = fromJust $ parseURI url
  result <- simpleHTTP $ mkRequest DELETE uri
  case result of
    Right (Response (2, _, _) _ _ body) -> logMsg NOTICE $ "Index deleted: " ++ body
    _                                   -> logMsg ERROR "Something went wrong in the index deletion. Is it already deleted?"

importFiles :: Opt.Options -> IO ()
importFiles opts = do
  let folder = Opt.folder opts
  logMsg NOTICE $ "Starting file imports from folder: " ++ folder ++ " ..."
  files <- (filter notTagFile) `liftM` getFilesRecurs folder
  mapM_ (importFile opts) files
  logMsg NOTICE $ show (length files) ++ " files imported successfully."

notTagFile :: FilePath -> Bool
notTagFile file = takeBaseName file /= "tags"

importFile :: Opt.Options -> FilePath -> IO ()
importFile opts file = do
  logMsg DEBUG $ "Importing File " ++ file
  doc <- createDoc opts file
  sendToES opts doc

createDoc :: Opt.Options -> FilePath -> IO G.Document
createDoc opts file = do
  let hash = getHash file
  tags     <- getTags file
  freeText <- case Opt.ocr opts of
                  True  -> extractText file
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
