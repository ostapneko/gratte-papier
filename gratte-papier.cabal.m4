define(`COMMON_DEPS', `base
                     , HTTP
                     , MissingH
                     , aeson
                     , attoparsec
                     , bytestring
                     , either
                     , hslogger
                     , mtl
                     , network
                     , old-time
                     , optparse-applicative
                     , process
                     , scotty
                     , split
                     , system-fileio
                     , system-filepath
                     , template-haskell
                     , temporary
                     , text
                     , unix
                     , vector
                     , wai-middleware-static')dnl
name:                gratte-papier
version:             0.2.0.0
synopsis:            Command-line tool to facilitate document archive via OCR.
license:             MIT
license-file:        LICENSE
author:              Thomas Franquelin
build-type:          Simple
cabal-version:       >=1.10

executable gratte-papier
  ghc-options:       -Wall -Werror
  main-is:           Main.hs
  hs-source-dirs:    src
  build-depends:     COMMON_DEPS
  default-language:  Haskell2010

test-suite integration
  type:              exitcode-stdio-1.0
  main-is:           Suite.hs
  hs-source-dirs:    tests/integration
                     , src
                     , tests/

  build-depends:     COMMON_DEPS
                     , hspec
