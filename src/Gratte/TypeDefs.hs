module Gratte.TypeDefs (
  module Gratte.TypeDefs
) where

newtype EsHost = EsHost String
newtype Prefix = Prefix String

data Mode = AddMode | QueryMode
