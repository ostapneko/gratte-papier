module Gratte.Add (
  addDocuments
  ) where

import Control.Monad

import System.FilePath
import System.Directory
import System.Process
import System.IO.Temp
import System.Exit
import System.Time
import System.Log.Logger

import Network.HTTP

import qualified Data.List     as L
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Data.Hash.MD5

import qualified Gratte.Options  as Opt
import qualified Gratte.TypeDefs as G
import           Gratte.Logger

addDocuments :: Opt.Options -> [G.Tag] -> [FilePath] -> IO ()
addDocuments opts tags files = do
  logAddFiles opts tags files

  forM_ files $ \file -> do
    fileIsNotDir <- doesFileExist file
    when fileIsNotDir $ do
      processFile file opts tags

processFile :: FilePath -> Opt.Options -> [G.Tag] -> IO ()
processFile file opts tags = do
  logMsg opts DEBUG $ "Processing file '" ++ file ++ "' ..."
  --create doc
  doc <- metadataToDoc opts tags file
  -- copy file
  copyToRepo opts file doc
  -- send to ElasticSearch
  sendToES opts doc

metadataToDoc :: Opt.Options -> [G.Tag] -> FilePath -> IO G.Document
metadataToDoc opts tags file = do
  let folder = Opt.folder opts
  let prf    = Opt.prefix opts
  hash       <- getHash
  let fp     = toNestedFilePath folder hash prf file
  freeText   <- case Opt.ocr opts of
                  True  -> extractText file
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

copyToRepo :: Opt.Options -> FilePath -> G.Document -> IO ()
copyToRepo opts file doc = do
  let newFile = G.filepath doc
  let dir = takeDirectory newFile
  logMsg opts DEBUG $ "\tCopy " ++ file ++ " to " ++ newFile
  guard (Opt.dryRun opts)
  createDirectoryIfMissing True dir
  copyFile file newFile
  forM_ (G.tags doc) $ \(G.Tag t) -> do
    appendFile (dir ++ "/tags") (t ++ "\n")

sendToES :: Opt.Options -> G.Document -> IO ()
sendToES opts doc = do
  let (G.EsHost esHost) = Opt.esHost opts
  let (G.Hash docId)    = G.hash doc
  let url = esHost ++ "/gratte/document/" ++ docId
  let payload = G.toPayload doc
  logMsg opts DEBUG $ "\tSending payload: " ++ G.toPayload doc
  guard (Opt.dryRun opts)
  _ <- simpleHTTP $ postRequestWithBody url "application/json" payload
  return ()

logAddFiles :: Opt.Options -> [G.Tag] -> [FilePath] -> IO ()
logAddFiles opts tags files = do
  logMsg opts DEBUG $ "Adding files \n\t"
                   ++ L.intercalate "\n\t" files
                   ++ "\nwith tags \n\t"
                   ++ L.intercalate "\n\t" (map G.toText tags)
                   ++ "\nStarting..."
