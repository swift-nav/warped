{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various alias types.
--
module Network.Warped.Types.Alias
  ( module Network.Warped.Types.Alias
  ) where

import Network.Warped.Prelude
import Network.Wai

-- | Respond
--
type Respond = Response -> IO ResponseReceived

-- | MonadApplication
--
type MonadApplication m = Request -> Respond -> m ResponseReceived
