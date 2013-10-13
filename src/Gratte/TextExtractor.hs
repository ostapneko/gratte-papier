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
import System.Exit
import System.FilePath
import System.Directory
import System.GratteExternalCommands

import Gratte.Options

type TextExtractor = FilePath -> FilePath -> Gratte (Maybe T.Text)

extractText :: FilePath -> Gratte (Maybe T.Text)
extractText file = do
  hasOcr <- getOption ocr
  pdfM   <- getOption pdfMode
  let ext = takeExtension file
  opts   <- getOptions
  liftIO $ withSystemTempDirectory "ocr-text" $ \tempDir -> do
    withGratte opts $
      case (ext, pdfM, hasOcr) of
        (".pdf", NoPDFMode   , False) -> handleNoPDFModeNoOCR
        (".pdf", NoPDFMode   , True ) -> handleNoPDFModeWithOCR tempDir file
        (".pdf", ImagePDFMode, _    ) -> extractPDFImage tempDir file
        (".pdf", TextPDFMode , _    ) -> extractPDFText tempDir file
        (_     , _           , False) -> return Nothing
        (_     , _           , True ) -> extractImage tempDir file

handleNoPDFModeNoOCR :: Gratte (Maybe T.Text)
handleNoPDFModeNoOCR = do
  logWarning "PDF file found but no PDF mode specified and no OCR... Assuming no text is to be extracted from the PDF."
  return Nothing

handleNoPDFModeWithOCR :: TextExtractor
handleNoPDFModeWithOCR tempDir file = do
  logWarning "PDF file found with OCR but no PDF mode specified... Fallback to image mode"
  extractPDFImage tempDir file

extractImage :: TextExtractor
extractImage tempDir file = do
  opts <- getOptions
  liftIO $ withTempFile tempDir "temp-ocr" $ \tempFile _ -> do
    (exitCode, err) <- execTesseract file tempFile
    case exitCode of
      ExitSuccess   -> do
        rawText <- TIO.readFile (tempFile ++ ".txt")
        return . Just $ T.map removeStrangeChars rawText
      ExitFailure _ -> do
        withGratte opts $ do
          logError $ "Could not OCR the file: " ++ file
          logError err
          return Nothing

extractPDFImage :: TextExtractor
extractPDFImage tempDir file = do
  opts <- getOptions
  liftIO $ do
    (exitCode, err) <- execConvert file tempDir
    withGratte opts $ do
      case exitCode of
        ExitSuccess   -> extractSingleImage tempDir
        ExitFailure _ -> do
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
    (exitCode, err) <- execConvertAppend images singleImage
    withGratte opts $ do
      case exitCode of
        ExitSuccess   -> extractImage tempDir singleImage
        ExitFailure _ -> do
          logError $ "Could OCR the image: " ++ singleImage
          logError err
          return Nothing

extractPDFText :: TextExtractor
extractPDFText tempDir file  = do
  opts <- getOptions
  liftIO $ withTempFile tempDir "temp-pdf-text.txt" $ \tempFile h -> do
    (exitCode, err) <- execPDFToText file tempFile
    case exitCode of
      ExitSuccess   -> do
        rawText <- TIO.hGetContents h
        return . Just $ T.map removeStrangeChars rawText
      ExitFailure _ -> do
        withGratte opts $ do
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
