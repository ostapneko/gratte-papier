module Gratte.SearchEngine
  ( tokenize
  , stem
  , insert
  , search
  , lookupDocument
  ) where

import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char

import Gratte.Document
import Gratte.Tag
import Control.Monad.Gratte
import qualified NLP.Tokenize.Text as Tok

tokenize :: Document -> S.Set T.Text
tokenize (Document _ (DocumentTitle title) _ (DocumentSender sender) (DocumentRecipient rec) _ tags scanned) =
  let metadata = map T.pack ([ title
                             , sender
                             , rec
                             ] ++ map unTag tags)
      texts = case scanned of
                Just t -> t : metadata
                _ -> metadata
  in S.fromList $ filter (T.any isAlphaNum) (texts >>= Tok.tokenize)

stem :: T.Text  -> T.Text
stem = undefined

insert :: DocumentPath -> S.Set T.Text -> Gratte ()
insert = undefined

lookupDocument :: DocumentPath -> Gratte (Maybe Document)
lookupDocument = undefined

search :: S.Set T.Text -> Gratte (S.Set DocumentPath)
search = undefined
