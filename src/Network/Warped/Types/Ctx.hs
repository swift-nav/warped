{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Contexts for monad transformers.
--
module Network.Warped.Types.Ctx
  ( module Network.Warped.Types.Ctx
  ) where

import Data.UUID
import Network.Wai
import Network.Warped.Prelude
import Network.Warped.Types.Alias

-- | WaiCtx
--
-- Context containing WAI application.
--
data WaiCtx = WaiCtx
  { _wcSessionUid :: UUID
    -- ^ Session Id.
  , _wcRequest    :: Request
    -- ^ WAI request.
  , _wcRespond    :: Respond
    -- ^ WAI response.
  }

$(makeClassy 'WaiCtx)

type MonadWai c m =
  ( MonadIO m
  , MonadReader c m
  , HasWaiCtx c
  )
