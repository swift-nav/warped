name:                  warped
version:               VERSION
synopsis:              Warp and Wai Library.
description:           Library support around WAI and warp server.
homepage:              https://github.com/swift-nav/warped
license:               MIT
license-file:          LICENSE
author:                Swift Navigation Inc.
maintainer:            Mark Fine <dev@swiftnav.com>
copyright:             Copyright (C) 2018 Swift Navigation, Inc.
category:              Network
build-type:            Simple
extra-source-files:    README.md
cabal-version:         >= 1.22

source-repository head
  type:                git
  location:            https://github.com/swift-nav/warped

library
  hs-source-dirs:      src
  exposed-modules:     Network.Warped
  other-modules:       Network.Warped.Application
                     , Network.Warped.Prelude
                     , Network.Warped.Types
                     , Network.Warped.Types.Alias
                     , Network.Warped.Types.Ctx
  ghc-options:         -Wall
  default-language:    Haskell2010
  build-depends:       aeson
                     , base >= 4.9 && < 5
                     , blaze-builder
                     , conduit
                     , http-types
                     , lifted-async
                     , monad-control
                     , preamble
                     , uuid
                     , wai
                     , wai-conduit
                     , wai-cors
                     , warp

executable shake-wolf
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  default-language:    Haskell2010
  build-depends:       base >= 4.9 && < 5
                     , shakers
