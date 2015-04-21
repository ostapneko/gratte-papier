define(`COMMON_DEPS', `base
                     , HTTP
                     , MissingH
                     , aeson
                     , attoparsec
                     , base16-bytestring
                     , bytestring
                     , cipher-aes
                     , DRBG
                     , either
                     , hslogger
                     , mtl
                     , network
                     , network-uri
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


test-suite unit
  type:              exitcode-stdio-1.0
  main-is:           Suite.hs
  hs-source-dirs:    tests/unit
                     , src
                     , tests/

  build-depends:     COMMON_DEPS
                     , hspec
