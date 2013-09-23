module Gratte.Add (
  addDocuments
  , sendToES
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Gratte
import Control.Concurrent

import System.FilePath
import System.Directory
import System.Time

import Network.HTTP

import qualified Data.List     as L
import           Data.Hash.MD5

import Gratte.Options
import Gratte.Document
import Gratte.TextExtractor (extractText)
import Gratte.Tag

-- | Add several documents with the same tags
addDocuments :: [FilePath] -> Gratte ()
addDocuments files = do
  ts <- getOption tags
  logStartAddingFiles ts files

  mvar <- liftIO $ newEmptyMVar
  existingFiles <- liftIO $ filterM doesFileExist files
  opts <- getOptions

  liftIO $ forM_ existingFiles $ \file -> forkIO $ do
    fileIsNotDir <- doesFileExist file
    when fileIsNotDir $ do
      gratte (processFile file ts) opts >> putMVar mvar ()

  liftIO $ replicateM_ (length existingFiles) (takeMVar mvar >> return ())
  logDoneAddingFiles files

-- | Add a single file with multiple tags
processFile :: FilePath -> [Tag] -> Gratte ()
processFile f ts = do
  logDebug $ "Processing file '" ++ f ++ "' ..."
  --create doc
  doc <- metadataToDoc ts f
  -- copy file
  copyToRepo f doc
  -- send to ElasticSearch
  sendToES doc

metadataToDoc :: [Tag] -> FilePath -> Gratte Document
metadataToDoc ts file = do
  f   <- getOption folder
  prf <- getOption prefix
  h   <- liftIO $ getHash
  let fp = toNestedFilePath f h prf file
  ft  <- extractText file
  return $ Document {
      docHash     = DocumentHash h
    , docFilepath = fp
    , docTags     = ts
    , docFreeText = ft
    }

getHash :: IO String
getHash = do
  TOD s ps <- getClockTime
  let timeStamp = show (s * 1000000000000 + ps)
  return $ md5s $ Str timeStamp

toNestedFilePath :: FilePath
                 -> String
                 -> Prefix
                 -> FilePath
                 -> FilePath
toNestedFilePath dir time (Prefix prf) f =
  let ext          = takeExtension f
      (a:b:c:rest) = time
  in dir
     </> [a] </> [b] </> [c]
     </> prf ++ "-" ++ rest ++ ext

copyToRepo :: FilePath -- ^ The path to the original file
           -> Document -- ^ The 'Document' representation of the file
           -> Gratte ()
copyToRepo file doc = do
  let newFile = docFilepath doc
  let dir = takeDirectory newFile
  logDebug $ "\tCopy " ++ file ++ " to " ++ newFile
  isDryRun <- getOption dryRun
  unless isDryRun $ liftIO $ do
    createDirectoryIfMissing True dir
    copyFile file newFile
    let metadataFile = replaceExtension newFile "metadata"
    appendFile metadataFile $ show doc

sendToES :: Document -> Gratte ()
sendToES doc = do
  (EsHost h)  <- getOption esHost
  (EsIndex i) <- getOption esIndex
  isDryRun    <- getOption dryRun
  let (DocumentHash docId) = docHash doc
  let url                  = h </> i </> "document" </> docId
  let payload              = toPayload doc
  logDebug $ "\tSending payload: " ++ toPayload doc
  unless isDryRun $ do
    _ <- liftIO . simpleHTTP $ postRequestWithBody url "application/json" payload
    return ()

logStartAddingFiles :: [Tag] -> [FilePath] -> Gratte ()
logStartAddingFiles ts fs = do
  logDebug $ "Adding f0iles \n\t"
          ++ L.intercalate "\n\t" fs
          ++ "\nwith tags \n\t"
          ++ L.intercalate "\n\t" (map toText ts)
  logNotice $ "Starting..."

logDoneAddingFiles :: [FilePath] -> Gratte ()
logDoneAddingFiles files = logNotice $
  "Done adding " ++ show (length files) ++ " files."
