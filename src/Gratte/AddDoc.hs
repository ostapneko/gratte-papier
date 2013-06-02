module Gratte.AddDoc (
  addDocuments
  ) where

import Control.Monad

import System.FilePath
import System.Directory
import System.Process
import System.IO.Temp
import System.Exit
import System.Time

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Hash.MD5

import qualified Gratte.Options             as Opt
import qualified Gratte.TypeDefs            as G

addDocuments :: Opt.Options -> [G.Tag] -> [FilePath] -> IO ()
addDocuments opts tags files = do
  forM_ files $ \file -> do
    fileIsNotDir <- doesFileExist file
    when fileIsNotDir $ do
      processFile file opts tags

processFile :: FilePath -> Opt.Options -> [G.Tag] -> IO ()
processFile file opts tags = do
  --create doc
  doc <- metadataToDoc opts tags file
  -- output JSON
  BS.putStrLn $ G.toByteString . G.BulkEntry $ doc
  -- copy file
  unless (Opt.dryRun opts) $ do
    let newFile = G.filepath doc
    createDirectoryIfMissing True $ takeDirectory newFile
    copyFile file newFile

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
        return $ T.filter (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']) rawText
      ExitFailure _ -> return T.empty
