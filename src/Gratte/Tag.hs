module Gratte.Tag where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T

newtype Tag = Tag { unTag :: String } deriving (Show, Eq, Read)

instance ToJSON Tag where
  toJSON (Tag t) = toJSON $ T.pack t

instance FromJSON Tag where
  parseJSON (String t) = return $ Tag $ T.unpack t
  parseJSON _          = mzero

toText :: Tag -> String
toText (Tag t) = t
