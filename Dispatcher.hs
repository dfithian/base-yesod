module Dispatcher where

import qualified API
import ClassyPrelude
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Logger (MonadLogger, logError)
import Foundation (App, RequestHandler(RequestHandler), appRequestQueue)
import Network.HTTP.Types (badRequest400, notFound404, internalServerError500)

apiBadRequest :: MonadBaseControl IO m => Text -> API.Result m a
apiBadRequest = throwError . (badRequest400,) . API.ErrorReason

apiNotFound :: MonadBaseControl IO m => Text -> API.Result m a
apiNotFound = throwError . (notFound404,) . API.ErrorReason

apiInternalError :: MonadBaseControl IO m => Text -> API.Result m a
apiInternalError = throwError . (internalServerError500,) . API.ErrorReason

-- |Run the dispatcher
--
-- This function runs ad infinitum for the life of the application. It forks a single request onto a new thread
-- and is automatically notified of new requests in the queue. Tries desperately not to fail.
runDispatcher :: (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadReader App m, MonadLogger m) => m ()
runDispatcher = do
  queue <- asks appRequestQueue
  void . fork . forever $ do
    RequestHandler request responder <- atomically $ readTBChan queue
    void . fork $
      handle (\ (se :: SomeException) -> $logError $ "Failed to dispatch request " <> tshow request <> " due to " <> tshow se) $
        runExceptT (dispatch request) >>= liftIO . responder

-- |The dispatch function
--
-- This function defines the process and restricts the response type for the given request. Thanks to a strong type
-- system, we define responses in terms of requests and the compiler takes care of the rest.
dispatch :: MonadIO m => API.Request a -> API.Result m (API.ResponseFor a)
dispatch = \ case
  API.PingRequest -> pure API.PingResponse
