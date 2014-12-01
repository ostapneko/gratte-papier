{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gratte.Document where

import           Data.Aeson
import qualified Data.Text                  as T
import qualified Data.List                  as L

import           Control.Applicative
import           Control.Monad

import qualified Filesystem.Path.CurrentOS as FS

import           Gratte.Tag
import           Gratte.TH

$(mkDocField "Hash")
$(mkDocField "Title")
$(mkDocField "Sender")
$(mkDocField "Recipient")

newtype DocumentPath = DocumentPath FS.FilePath deriving Show

instance ToJSON DocumentPath where
  toJSON (DocumentPath path) = toJSON . FS.encodeString $ path

instance FromJSON DocumentPath where
  parseJSON (String path) = return $ DocumentPath $ FS.fromText path
  parseJSON _ = mzero

docPathToString :: DocumentPath -> String
docPathToString (DocumentPath path) = FS.encodeString path

data Month = January | February | March | April | May | June | July
           | August | September | October | November | December
           deriving (Show, Read, Enum)

instance ToJSON Month where
  toJSON = toJSON . show

instance FromJSON Month where
  parseJSON (String m) =
    case reads (T.unpack m) of
      [(month, "")] -> return month
      _             -> mzero
  parseJSON _ = mzero

data DocumentDate = DocumentDate (Maybe Month) Integer deriving Show
instance ToJSON DocumentDate where
  toJSON (DocumentDate mMonth year) =
    object [ "month" .= toJSON mMonth
           , "year"  .= toJSON year
           ]

instance FromJSON DocumentDate where
  parseJSON (Object v) =
    DocumentDate <$> v .:? "month"
                 <*> v .: "year"
  parseJSON _ = mzero

docDateToString :: DocumentDate -> String
docDateToString (DocumentDate mMonth year) =
  let monthString = case mMonth of
        Nothing -> ""
        Just month -> show month ++ " "
  in monthString ++ show year

data Document = Document {
    docHash        :: DocumentHash
  , docTitle       :: DocumentTitle
  , docPath        :: DocumentPath
  , docSender      :: DocumentSender
  , docRecipient   :: DocumentRecipient
  , docDate        :: DocumentDate
  , docTags        :: [Tag]
  , docScannedText :: Maybe T.Text
  } deriving Show

instance ToJSON Document where
  toJSON doc =
    object [ "hash"        .= docHash doc
           , "title"       .= docTitle doc
           , "path"        .= docPath doc
           , "sender"      .= docSender doc
           , "recipient"   .= docRecipient doc
           , "date"        .= docDate doc
           , "tags"        .= docTags doc
           , "scannedText" .= docScannedText doc
           ]

instance FromJSON Document where
  parseJSON (Object v) = do
    hash        <- v .: "hash"
    title       <- v .: "title"
    path        <- v .: "path"
    sender      <- v .: "sender"
    recipient   <- v .: "recipient"
    date        <- v .: "date"
    tags        <- v .: "tags"
    scannedText <- v .: "scannedText"
    return Document
      { docHash        = hash
      , docTitle       = title
      , docPath        = path
      , docSender      = sender
      , docRecipient   = recipient
      , docDate        = date
      , docTags        = tags
      , docScannedText = scannedText
      }
  parseJSON _          = mzero

newtype DocumentPayload = DocumentPayload Document deriving (Show)
instance ToJSON DocumentPayload where
  toJSON (DocumentPayload doc) = toJSON doc
instance FromJSON DocumentPayload where
  parseJSON (Object v) = do
    source <- v .: "_source"
    doc <- parseJSON source
    return $ DocumentPayload doc
  parseJSON _ = mzero

data SearchResult = SearchResult Hits
instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "hits"
  parseJSON _          = mzero

data Hits = Hits [DocumentPayload]
instance FromJSON Hits where
  parseJSON (Object v) = Hits <$> v .: "hits"
  parseJSON _          = mzero

createReport :: FS.FilePath -> [Document] -> String
createReport docFolder docs =
  let firstLine = "You are about to archive the following files:"
      docDescriptions = map (describeDoc docFolder) docs
  in unlines $ firstLine : docDescriptions

describeDoc :: FS.FilePath -> Document -> String
describeDoc docFolder doc =
  let  text = maybe "(no scanned text)" T.unpack $ docScannedText doc
       tags = docTags doc
       date = docDateToString $ docDate doc
       docFolder' = FS.encodeString docFolder
  in "--------------------------------\n"
    ++ "Title: "        ++ docTitleToString (docTitle doc)             ++ "\n"
    ++ "Path: "         ++ docFolder' ++ docPathToString (docPath doc) ++ "\n"
    ++ "Sender: "       ++ docSenderToString (docSender doc)           ++ "\n"
    ++ "Recipient: "    ++ docRecipientToString (docRecipient doc)     ++ "\n"
    ++ "Date: "         ++ date                                        ++ "\n"
    ++ "Tags: "         ++ L.intercalate ", " (map toText tags)        ++ "\n"
    ++ "Scanned text: " ++ text                                        ++ "\n"
