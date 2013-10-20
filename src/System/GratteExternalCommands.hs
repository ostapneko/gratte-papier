module System.GratteExternalCommands
  ( execTesseract
  , execConvert
  , execConvertAppend
  , execPDFToText
  ) where

import System.Process
import System.Exit

execTesseract :: FilePath -> FilePath -> IO (ExitCode, String)
execTesseract file tempFile = do
  (exitCode, _, err) <- do
    readProcessWithExitCode
      "tesseract"
      [file, tempFile]
      ""
  return (exitCode, err)

execConvert :: FilePath -> FilePath -> IO (ExitCode, String)
execConvert file tempDir = do
  (exitCode, _, err) <- do
    readProcessWithExitCode
      "convert"
      [file, tempDir ++ "/temp-png.png"]
      ""
  return (exitCode, err)

execConvertAppend :: [FilePath] -> FilePath -> IO (ExitCode, String)
execConvertAppend images singleImage = do
  (exitCode, _, err) <- do
    readProcessWithExitCode
      "convert"
      (images ++ ["-append", singleImage])
      ""
  return (exitCode, err)

execPDFToText :: FilePath -> FilePath -> IO (ExitCode, String)
execPDFToText file tempFile = do
  (exitCode, _, err) <- do
    readProcessWithExitCode
      "pdftotext"
      [file, tempFile]
      ""
  return (exitCode, err)
