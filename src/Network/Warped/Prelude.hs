{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.Warped.Prelude
  ( module Exports
  , lookupDefault
  ) where

import Preamble as Exports

-- | lookup with a default.
--
lookupDefault :: Eq a => b -> a -> [(a, b)] -> b
lookupDefault b a m = fromMaybe b $ lookup a m

