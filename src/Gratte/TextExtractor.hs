{-# LANGUAGE OverloadedStrings #-}

module Gratte.TextExtractor
  ( extractText
  ) where

import           Control.Monad
import           Control.Monad.Gratte
import           Control.Monad.Trans

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           System.Exit
import           System.GratteExternalCommands
import           System.IO.Temp

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

import           Gratte.Options
import           Gratte.Utils

type TextExtractor = FilePath -> FilePath -> Gratte (Maybe T.Text)

extractText :: FS.FilePath -> Gratte (Maybe T.Text)
extractText file = do
  hasOcr <- getAddOption ocr
  pdfM   <- getAddOption pdfMode
  let ext = FS.extension file
  opts   <- getOptions
  let filePath = FS.encodeString file
  liftIO $ withSystemTempDirectory "ocr-text" $ \ tempDir ->
    withGratte opts $
      case (ext, pdfM, hasOcr) of
        (Just "pdf", PDFModeImage, _    )  -> extractPDFImage tempDir filePath
        (Just "pdf", PDFModeText , _    )  -> extractPDFText tempDir filePath
        (_          , _           , False) -> return Nothing
        (_          , _           , True ) -> extractImage tempDir filePath

extractImage :: TextExtractor
extractImage tempDir file = do
  opts <- getOptions
  let tmpFileNoExt = tempDir ++ "/tmp"
  liftIO $ do
    (exitCode, err) <- execTesseract file tmpFileNoExt
    case exitCode of
      ExitSuccess   -> do
        -- caveat: tesseract adds a .txt extension to the output, even if it already exists!
        rawText <- TIO.readFile (tmpFileNoExt ++ ".txt")
        return . Just $ T.map removeStrangeChars rawText
      ExitFailure _ ->
        withGratte opts $ do
          logError $ "Could not OCR the file: " ++ file
          logError err
          return Nothing

extractPDFImage :: TextExtractor
extractPDFImage tempDir file = do
  opts <- getOptions
  liftIO $ do
    (exitCode, err) <- execConvert file tempDir
    withGratte opts $
      case exitCode of
        ExitSuccess   -> extractSingleImage (FS.decodeString tempDir)
        ExitFailure _ -> do
          logError $ "Could not convert the PDF file to image: " ++ file
          logError err
          return Nothing

extractSingleImage :: FS.FilePath -> Gratte (Maybe T.Text)
extractSingleImage tempDir = do
  let singleImage = tempDir <//> "single-image.png"
  opts <- getOptions
  liftIO $ do
    imagesBaseNames <- filter (`FS.hasExtension` "png") `liftM` FS.listDirectory tempDir
    let imagePaths = map FS.encodeString imagesBaseNames
        singleImagePath = FS.encodeString singleImage
    (exitCode, err) <- execConvertAppend imagePaths singleImagePath
    withGratte opts $
      case exitCode of
        ExitSuccess   -> extractImage (FS.encodeString tempDir) singleImagePath
        ExitFailure _ -> do
          logError $ "Could OCR the image: " ++ singleImagePath
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
      ExitFailure _ ->
        withGratte opts $ do
          logError $ "Could not convert the pdf file: " ++ file
          logError err
          return Nothing

removeStrangeChars :: Char -> Char
removeStrangeChars c =
  if c `elem` alpha then c else ' '
    where alpha = ['a'..'z'] ++ ['A'..'Z'] ++
                  ['0'..'9'] ++ "ÉÈÊÀÂÎÔéèêàâîô."
