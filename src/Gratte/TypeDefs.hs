module Gratte.TypeDefs (
  module Gratte.TypeDefs
) where

data Options = Options {
    verbose         :: Bool
  , silent          :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    verbose         = False
  , silent          = False
}
