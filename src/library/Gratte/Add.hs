module Gratte.Add (
  addDocuments
  , sendToES
  , extractText
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte

import System.FilePath
import System.Directory
import System.Process
import System.IO.Temp
import System.Exit
import System.Time

import Network.HTTP

import qualified Data.List     as L
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Data.Hash.MD5

import qualified Gratte.TypeDefs as G
import qualified Gratte.Options  as O

addDocuments :: [G.Tag] -> [FilePath] -> Gratte ()
addDocuments tags files = do
  logStartAddingFiles tags files

  forM_ files $ \file -> do
    fileIsNotDir <- liftIO $ doesFileExist file
    when fileIsNotDir $ do
      processFile file tags

  logDoneAddingFiles files

processFile :: FilePath -> [G.Tag] -> Gratte ()
processFile file tags = do
  logDebug $ "Processing file '" ++ file ++ "' ..."
  --create doc
  doc <- metadataToDoc tags file
  -- copy file
  copyToRepo file doc
  -- send to ElasticSearch
  sendToES doc

metadataToDoc :: [G.Tag] -> FilePath -> Gratte G.Document
metadataToDoc tags file = do
  folder <- getOption O.folder
  prf    <- getOption O.prefix
  hash   <- liftIO $ getHash
  let fp = toNestedFilePath folder hash prf file
  useOcr <- getOption O.ocr
  freeText   <- case useOcr of
                  True  -> liftIO $ extractText file
                  False -> return T.empty
  return $ G.Document {
      G.hash      = G.Hash hash
    , G.filepath  = fp
    , G.tags      = tags
    , G.freeText  = freeText
    }

getHash :: IO String
getHash = do
  TOD s ps <- getClockTime
  let timeStamp = show (s * 1000000000000 + ps)
  return $ md5s $ Str timeStamp

toNestedFilePath :: FilePath
                 -> String
                 -> G.Prefix
                 -> FilePath
                 -> FilePath
toNestedFilePath folder time (G.Prefix prf) file =
  let ext          = takeExtension file
      (a:b:c:rest) = time
  in folder ++ "/"
     ++ [a] ++ "/" ++ [b] ++ "/" ++ [c] ++ "/"
     ++ prf ++ "-" ++ rest ++ ext

extractText :: FilePath -> IO T.Text
extractText file = do
  withSystemTempFile "ocr-text" $ \path _ -> do
    (exitCode, _, _) <- do
      readProcessWithExitCode
        "tesseract"
        [file, path]
        ""
    case exitCode of
      ExitSuccess   -> do
        rawText <- TIO.readFile (path ++ ".txt")
        return $ T.map removeStrangeChars rawText
      ExitFailure _ -> return T.empty

removeStrangeChars :: Char -> Char
removeStrangeChars c =
  case c `elem` alpha of
      True  -> c
      False -> ' '
    where alpha = ['a'..'z'] ++ ['A'..'Z'] ++
                  ['0'..'9'] ++ "ÉÈÊÀÂÎÔéèêàâîô."

copyToRepo :: FilePath -> G.Document -> Gratte ()
copyToRepo file doc = do
  let newFile = G.filepath doc
  let dir = takeDirectory newFile
  logDebug $ "\tCopy " ++ file ++ " to " ++ newFile
  isDryRun <- getOption O.dryRun
  unless isDryRun $ liftIO $ do
    createDirectoryIfMissing True dir
    copyFile file newFile
    forM_ (G.tags doc) $ \(G.Tag t) -> do
      appendFile (dir ++ "/tags") (t ++ "\n")

sendToES :: G.Document -> Gratte ()
sendToES doc = do
  (G.EsHost esHost) <- getOption O.esHost
  isDryRun <- getOption O.dryRun
  let (G.Hash docId) = G.hash doc
  let url = esHost ++ "/gratte/document/" ++ docId
  let payload = G.toPayload doc
  logDebug $ "\tSending payload: " ++ G.toPayload doc
  unless isDryRun $ do
    _ <- liftIO . simpleHTTP $ postRequestWithBody url "application/json" payload
    return ()

logStartAddingFiles :: [G.Tag] -> [FilePath] -> Gratte ()
logStartAddingFiles tags files = do
  logDebug $ "Adding files \n\t"
               ++ L.intercalate "\n\t" files
               ++ "\nwith tags \n\t"
               ++ L.intercalate "\n\t" (map G.toText tags)
  logNotice $ "Starting..."

logDoneAddingFiles :: [FilePath] -> Gratte ()
logDoneAddingFiles files = logNotice $
  "Done adding " ++ show (length files) ++ " files."
