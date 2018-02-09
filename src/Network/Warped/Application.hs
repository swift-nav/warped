{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Wai and Warp helpers.
--
module Network.Warped.Application
  ( flushBuilder
  , requestHeader
  , requestQuery
  , warp
  , warpCors
  , raceResponse
  ) where

import Blaze.ByteString.Builder
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control
import Data.Conduit
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Warped.Prelude
import Network.Warped.Types

-- | A Conduit for converting a ByteString to a Flush Builder.
--
flushBuilder :: Monad m => Conduit ByteString m (Flush Builder)
flushBuilder = awaitForever $ \chunk -> do
  yield $ Chunk $ fromByteString chunk
  yield Flush

-- | Lookup a request header.
--
requestHeader :: HeaderName -> Request -> Maybe ByteString
requestHeader header = lookup header . requestHeaders

-- | Look up a query parameter.
--
requestQuery :: ByteString -> Request -> Maybe ByteString
requestQuery key = join . lookup key . queryString

-- | Run warp with lifted type.
--
warp :: (MonadBaseControl IO m, StM m ResponseReceived ~ ResponseReceived) => Settings -> MonadApplication m -> m ()
warp settings app =
  liftBaseWith $ \runInIO ->
    runSettings settings $ \request response ->
      runInIO $ app request response

-- | Run warp with lifted type.
--
warpCors :: (MonadBaseControl IO m, StM m ResponseReceived ~ ResponseReceived) => Settings -> CorsResourcePolicy -> MonadApplication m -> m ()
warpCors settings policy app =
  liftBaseWith $ \runInIO ->
    runSettings settings $
      cors (const $ pure policy) $ \request response ->
        runInIO $ app request response

-- | Race between source and response - return whoever finishes first.
--
raceResponse :: MonadBaseControl IO m => m ResponseReceived -> m ResponseReceived -> m ResponseReceived
raceResponse a b = race a b >>= either pure pure
