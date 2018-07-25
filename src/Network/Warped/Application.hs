{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Wai and Warp helpers.
--
module Network.Warped.Application
  ( flushBuilder
  , requestHeader
  , requestQuery
  , warp
  , warpCors
  , route
  , routeMethod
  , routePath
  , raceResponse
  , answer
  , answerStatus
  , answerSource
  , answerJson
  , withHeader
  ) where

import Blaze.ByteString.Builder
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Conduit
import Data.UUID                       hiding (fromByteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Warped.Prelude
import Network.Warped.Types

-- | Session-Uid Header
--
hSessionUid :: HeaderName
hSessionUid = "Session-Uid"

-- | A Conduit for converting a ByteString to a Flush Builder.
--
flushBuilder :: Monad m => ConduitT ByteString (Flush Builder) m ()
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

-- | General purpose router on requests.
--
route :: (MonadWai c m, Eq a) => (Request -> a) -> m ResponseReceived -> [(a, m ResponseReceived)] -> m ResponseReceived
route a b routes = do
  request <- view wcRequest
  lookupDefault b (a request) routes

-- | Route methods
--
routeMethod :: MonadWai c m => m ResponseReceived -> [(Method, m ResponseReceived)] -> m ResponseReceived
routeMethod = route requestMethod

-- | Route paths.
--
routePath :: MonadWai c m => m ResponseReceived -> [(ByteString, m ResponseReceived)] -> m ResponseReceived
routePath = route rawPathInfo

-- | All responses.
--
answer :: MonadWai c m => Response -> m ResponseReceived
answer response = do
  respond <- view wcRespond
  liftIO $ respond response

-- | Status response.
--
answerStatus :: MonadWai c m => Status -> ResponseHeaders -> m ResponseReceived
answerStatus status headers = do
  sessionUid <- view wcSessionUid
  let headers' = hSessionUid =. toASCIIBytes sessionUid : headers
  answer $ responseLBS status headers' mempty

-- | Stream response.
--
answerSource :: MonadWai c m => Status -> ResponseHeaders -> ConduitT () (Flush Builder) IO () -> m ResponseReceived
answerSource status headers response = do
  sessionUid <- view wcSessionUid
  let headers' = hSessionUid =. toASCIIBytes sessionUid : headers
  answer $ responseSource status headers' response

-- | JSON response.
--
answerJson :: (MonadWai c m, ToJSON a) => Status -> ResponseHeaders -> a -> m ResponseReceived
answerJson status headers value = do
  sessionUid <- view wcSessionUid
  let headers' = hSessionUid =. toASCIIBytes sessionUid : headers
  answer $ responseLBS status headers' $ encode value

-- | Lookup header.
--
withHeader :: MonadWai c m => HeaderName -> (HeaderName -> m ResponseReceived) -> (ByteString -> m ResponseReceived) -> m ResponseReceived
withHeader header noaction action = do
  request <- view wcRequest
  maybe (noaction header) action $ requestHeader header request
