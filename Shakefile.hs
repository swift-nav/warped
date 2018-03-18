#!/usr/bin/env stack
{- stack
    runghc
    --package shakers
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import Development.Shakers

-- | Main entry point.
--
main :: IO ()
main = shakeMain $ do
  let pats =
        [ "stack*.yaml"
        , "Shakefile.hs"
        , "src//*.hs"
        ]
      pats' = delete "stack*.yaml" pats

  -- | Haskell rules.
  --
  hsRules "." pats'

  -- | Cabal rules.
  --
  cabalRules "." "warped.cabal"

  -- | Stack rules.
  --
  stackRules "." pats

  -- | sanity
  --
  fake "." pats "sanity" $ const $
    need [ "build-error", "lint" ]

  -- | Default things to run.
  --
  want [ "sanity", "format" ]
