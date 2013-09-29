{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gratte.Document where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import qualified Data.List                  as L

import           Control.Applicative
import           Control.Monad

import           Gratte.Tag
import           Gratte.TH

$(mkDocField "Hash")
$(mkDocField "Title")
$(mkDocField "Path")
$(mkDocField "Sender")
$(mkDocField "Recipient")

data Month = January | February | March | April | May | June | July
           | August | September | October | November | December
           deriving (Show, Read)
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
    object $ [ "month" .= toJSON mMonth
             , "year"  .= toJSON year
             ]

instance FromJSON DocumentDate where
  parseJSON (Object v) = do
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
  , docDate        :: Maybe DocumentDate
  , docTags        :: [Tag]
  , docScannedText :: Maybe T.Text
  } deriving Show

instance ToJSON Document where
  toJSON doc =
    object $ [ "hash"        .= docHash doc
             , "title"       .= docTitle doc
             , "path"        .= docPath doc
             , "sender"      .= docSender doc
             , "recipient"   .= docRecipient doc
             , "date"        .= docDate doc
             , "tags"        .= docTags doc
             , "scannedText" .= docScannedText doc
             ]

toPayload :: Document -> String
toPayload = BS.unpack . encode . toJSON

instance FromJSON Document where
  parseJSON (Object v) = do
    source      <- v .: "_source" :: Parser Object
    hash        <- source .: "hash"
    title       <- source .: "title"
    path        <- source .: "path"
    sender      <- source .: "sender"
    recipient   <- source .: "recipient"
    date        <- source .: "date"
    tags        <- source .: "tags"
    scannedText <- source .: "scannedText"
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

data SearchResult = SearchResult Hits
instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "hits"
  parseJSON _          = mzero

data Hits = Hits [Document]
instance FromJSON Hits where
  parseJSON (Object v) = Hits <$> v .: "hits"
  parseJSON _          = mzero

createReport :: [Document] -> String
createReport docs =
  let firstLine = "You are about to archive the following files:"
      docDescriptions = map describeDoc docs
  in unlines $ firstLine : docDescriptions

describeDoc :: Document -> String
describeDoc doc =
  let  text = maybe "(no scanned text)" T.unpack $ docScannedText doc
       tags = docTags doc
       date = maybe "(no date)" docDateToString $ docDate doc
  in "--------------------------------\n"
    ++ "Title: "        ++ docTitleToString (docTitle doc)         ++ "\n"
    ++ "Sender: "       ++ docSenderToString (docSender doc)       ++ "\n"
    ++ "Recipient: "    ++ docRecipientToString (docRecipient doc) ++ "\n"
    ++ "Date: "         ++ date                                    ++ "\n"
    ++ "Tags: "         ++ L.intercalate ", " (map toText tags)    ++ "\n"
    ++ "Scanned text: " ++ text                                    ++ "\n"
