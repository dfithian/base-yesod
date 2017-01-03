module Foundation where

import qualified API
import ClassyPrelude
import Control.Lens (_2, _Left, _Right, over)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Data.Aeson (Value, toJSON)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Network.HTTP.Types (Status, ok200)
import Settings (AppSettings)
import Yesod.Core (Yesod, renderRoute)
import Yesod.Core.Dispatch (mkYesodData, parseRoutesFile)

-- |The application, whose members here are computed at runtime
data App = App
  { appRequestQueue   :: TBChan RequestHandler
  -- ^ The queue for requests coming in, which are dispatched in "Dispatcher"
  , appConnectionPool :: ConnectionPool
  -- ^ The PostgreSQL connection pool
  , appSettings       :: AppSettings
  -- ^ Settings as defined in "Settings"
  , appLog            :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  -- ^ The logging function for the application, the very same that Yesod uses
  }

-- |A request handler
--
-- Pairs the request with a function that writes to a mutable cell so that processing can occur asynchronously
data RequestHandler = forall a . RequestHandler
  { request         :: API.Request a
  -- ^ The request
  , responseHandler :: Either (Status, API.ErrorReason) (API.ResponseFor a) -> IO ()
  -- ^ The response handler
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

-- |Dispatch a request by writing it to the queue
--
-- Processing is handled in the "Dispatcher" module, which checks for new requests many times a second
dispatchRequest :: (MonadBaseControl IO m, MonadIO m, MonadReader App m) => API.Request a -> m (Status, Value)
dispatchRequest request = do
  queue <- asks appRequestQueue
  tmv <- liftIO newEmptyTMVarIO
  -- provide access to the mutable cell so that another thread can write to it
  liftIO . atomically . writeTBChan queue . RequestHandler request $ atomically . putTMVar tmv
  either id id
    . over _Right ((ok200,) . API.responseToJSON request)
    . over (_Left . _2) toJSON
    <$> liftIO (atomically $ takeTMVar tmv)

-- |Run a database action using the connection pool
runDb :: (MonadBaseControl IO m, MonadReader App m) => SqlPersistT m a -> m a
runDb action = do
  pool <- asks appConnectionPool
  runSqlPool action pool
