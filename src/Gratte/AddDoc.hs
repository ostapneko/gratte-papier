module Gratte.AddDoc (
  addDocuments
  ) where

import Control.Monad


import System.IO
import System.FilePath
import System.Directory
import System.Process
import System.IO.Temp
import System.Exit
import System.Time

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import qualified Gratte.Options             as Opt
import qualified Gratte.TypeDefs            as G

addDocuments :: Opt.Options -> [G.Tag] -> [FilePath] -> IO ()
addDocuments opts tags files = do
  unless (Opt.silent opts) (hPutStrLn stderr $ "Sending files to ElasticSearch ...")
  forM_ files $ \file -> do
    fileIsNotDir <- doesFileExist file
    when fileIsNotDir $ do
      --create doc
      doc <- metadataToDoc opts tags file
      -- output JSON
      BS.putStrLn $ G.toByteString . G.BulkEntry $ doc
      -- copy file
      let newFile = G.filepath doc
      createDirectoryIfMissing True $ takeDirectory newFile
      copyFile file newFile
  unless (Opt.silent opts) (hPutStrLn stderr $ "Files sent ElasticSearch")

metadataToDoc :: Opt.Options -> [G.Tag] -> FilePath -> IO G.Document
metadataToDoc opts tags file = do
  let folder = Opt.folder opts
  let prf    = Opt.prefix opts
  time       <- getTimestamp
  let fp     = toNestedFilePath folder time prf file
  freeText   <- extractText file
  return $ G.Document {
      G.timestamp = G.Timestamp time
    , G.filepath  = fp
    , G.tags      = tags
    , G.freeText  = freeText
    }

getTimestamp :: IO String
getTimestamp = do
  TOD s ps <- getClockTime
  return $ show (s * 1000000000000 + ps)

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
