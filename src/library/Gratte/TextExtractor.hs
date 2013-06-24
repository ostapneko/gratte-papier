{-# LANGUAGE OverloadedStrings #-}

module Gratte.TextExtractor
  ( extractText
  ) where

import Control.Monad
import Control.Monad.Gratte
import Control.Monad.Trans

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.List    as L

import System.IO.Temp
import System.Process
import System.Exit
import System.FilePath
import System.Directory

import Gratte.Options

type TextExtractor = FilePath -> FilePath -> Gratte (Maybe T.Text)

extractText :: FilePath -> Gratte (Maybe T.Text)
extractText file = do
  hasOcr <- getOption ocr
  pdfM   <- getOption pdfMode
  let ext = takeExtension file
  opts   <- getOptions
  liftIO $ withSystemTempDirectory "ocr-text" $ \tempDir -> do
    flip gratte opts $
      case (ext, pdfM, hasOcr) of
        (".pdf", NoPDFMode, False) -> do
          logWarning "PDF file found but no PDF mode specified and no OCR... Assuming no text is to be extracted from the PDF."
          return Nothing
        (".pdf", NoPDFMode, True) -> do
          logWarning "PDF file found with OCR but no PDF mode specified... Fallback to image mode"
          extractPDFImage tempDir file
        (".pdf", ImagePDFMode, _) -> do
          extractPDFImage tempDir file
        (".pdf", TextPDFMode, _) -> do
          extractPDFText tempDir file
        (_, _, False) -> return Nothing
        (_, _, True) -> do
          extractImage tempDir file

extractImage :: TextExtractor
extractImage tempDir file = do
  opts <- getOptions
  liftIO $ withTempFile tempDir "temp-ocr" $ \tempFile _ -> do
    (exitCode, _, err) <- do
      readProcessWithExitCode
        "tesseract"
        [file, tempFile]
        ""
    case exitCode of
      ExitSuccess   -> do
        rawText <- TIO.readFile (tempFile ++ ".txt")
        return . Just $ T.map removeStrangeChars rawText
      ExitFailure _ -> do
        flip gratte opts $ do
          logError $ "Could not OCR the file: " ++ file
          logError err
          return Nothing

extractPDFImage :: TextExtractor
extractPDFImage tempDir file = do
  opts <- getOptions
  liftIO $ do
    (exitCode, _, err) <- do
      readProcessWithExitCode
        "convert"
        [file, tempDir </> "temp-png.png"]
        ""
    case exitCode of
      ExitSuccess -> flip gratte opts $ do
                       mText <- extractSingleImage tempDir
                       return mText
      ExitFailure _ -> do
        flip gratte opts $ do
          logError $ "Could not convert the PDF file to image: " ++ file
          logError err
          return Nothing

extractSingleImage :: FilePath -> Gratte (Maybe T.Text)
extractSingleImage tempDir = do
  let singleImage = tempDir </> "single-image.png"
  opts <- getOptions
  liftIO $ do
    imagesBaseNames <- filter (L.isSuffixOf ".png") `liftM` (getDirectoryContents tempDir)
    let images = map (\ n -> tempDir </> n) imagesBaseNames
    (exitCode, _, err) <- do
      readProcessWithExitCode
        "convert"
        (images ++ ["-append", singleImage])
        ""
    case exitCode of
      ExitSuccess -> flip gratte opts $ do
                       mText <- extractImage tempDir singleImage
                       return mText
      ExitFailure _ -> do
        flip gratte opts $ do
          logError $ "Could OCR the image: " ++ singleImage
          logError err
          return Nothing

extractPDFText :: TextExtractor
extractPDFText tempDir file  = do
  opts <- getOptions
  liftIO $ withTempFile tempDir "temp-pdf-text.txt" $ \tempFile h -> do
    (exitCode, _, err) <- do
      readProcessWithExitCode
        "pdftotext"
        [file, tempFile]
        ""
    case exitCode of
      ExitSuccess   -> do
        rawText <- TIO.hGetContents h
        return . Just $ T.map removeStrangeChars rawText
      ExitFailure _ -> do
        flip gratte opts $ do
          logError $ "Could not convert the pdf file: " ++ file
          logError err
          return Nothing

removeStrangeChars :: Char -> Char
removeStrangeChars c =
  case c `elem` alpha of
      True  -> c
      False -> ' '
    where alpha = ['a'..'z'] ++ ['A'..'Z'] ++
                  ['0'..'9'] ++ "ÉÈÊÀÂÎÔéèêàâîô."
