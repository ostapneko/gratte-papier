module Gratte.Options.Add where

import           Data.Char
import           Data.List

import           Gratte.Tag
import           Gratte.Document
import           Options.Applicative

import qualified Data.List.Split as SPL
import qualified Filesystem.Path.CurrentOS as FS

data AddOptions = AddOptions
  { pdfMode   :: PDFMode
  , ocr       :: Bool
  , title     :: DocumentTitle
  , sender    :: DocumentSender
  , recipient :: DocumentRecipient
  , date      :: DocumentDate
  , tags      :: [Tag]
  , newFiles  :: [FS.FilePath]
  } deriving Show

data PDFMode = PDFModeImage
             | PDFModeText
             deriving Show

parseAddOptions :: Parser AddOptions
parseAddOptions = AddOptions
              <$> flag PDFModeImage PDFModeText
                  ( long "text-pdf"
                 <> help "Use this switch if the document to add is a PDF and not an image (copy-pastable text)" )
              <*> flag False True
                  ( long "ocr"
                 <> short 'o'
                 <> help "Uses OCR to try extract the text from the documents and add it as searchable metadata. Requires tesseract to be installed." )
              <*> option (DocumentTitle <$> str)
                  ( long "title"
                 <> short 't'
                 <> metavar "TITLE"
                 <> help "The title of the documents. If more than one documents are present, add a page number after it (e.g. \"My doc (Page 1)\", etc. )")
              <*> option (DocumentSender <$> str)
                  ( long "sender"
                 <> short 's'
                 <> metavar "NAME"
                 <> help "The document's sender")
              <*> option (DocumentRecipient <$> str)
                  ( long "recipient"
                 <> short 'r'
                 <> metavar "NAME"
                 <> help "The document's recipient")
              <*> option (str >>= parseDate)
                  ( long "date"
                 <> short 'd'
                 <> metavar "\"MONTH YEAR\""
                 <> help "The documents' month (optionaly) and year. If provided, the date MUST be in the form \"September 2013\".")
              <*> option (str >>= parseTags)
                  ( long "tags"
                 <> short 'T'
                 <> metavar "TAG1,TAG2"
                 <> value []
                 <> help "Add a comma or colon separated list of tags to the document")
              <*> many (
                    argument (FS.decodeString <$> str)
                             ( help "Files to add"
                             <> metavar "FILES" )
                  )

parseDate :: Monad m => String -> m DocumentDate
parseDate arg =
  let (lhs, rhs) = (\ (x, y) -> (x, drop 1 y)) . break (==' ') $ arg
      (eMonth, eYear) = case (lhs, rhs) of
        (_, "") -> (Right Nothing, parseYear lhs)
        _       -> (parseMonth lhs, parseYear rhs)
  in case (eMonth, eYear) of
    (Left failure, _)          -> fail failure
    (_, Left failure)          -> fail failure
    (Right mMonth, Right year) -> return $ DocumentDate mMonth year

parseMonth :: String -> Either String (Maybe Month)
parseMonth "" = Right Nothing
parseMonth (h : t) = Just <$> monthStartingWith (toUpper h : t)

monthStartingWith :: String -> Either String Month
monthStartingWith s =
  let months = enumFrom January
      ms = filter (isPrefixOf s . show) months
  in case ms of
    [m] -> Right m
    []  -> Left "Please enter a valid month"
    _   -> Left $ "Ambiguous month. It could be " ++ intercalate ", " (map show ms)

parseYear :: String -> Either String Integer
parseYear inputYear =
  case reads inputYear :: [(Integer, String)] of
    [(year, "")] -> Right year
    _            -> Left "Please enter a valid year"

parseTags :: Monad m => String -> m [Tag]
parseTags = return . map (Tag . dropWhile (==' ')) . SPL.splitOneOf ",:"
