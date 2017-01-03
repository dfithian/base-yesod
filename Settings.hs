module Settings where

import ClassyPrelude
import Data.Aeson (FromJSON(parseJSON), withObject, (.:?))

-- |The settings for the application
data AppSettings = AppSettings
  { appMaxWorkers             :: Int
  -- ^ Maximum workers to use for request processing
  , appMaxDbConnections       :: Int
  -- ^ Maximum database connections allowed
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \ obj ->
    AppSettings <$> (fromMaybe 10 <$> obj .:? "max-workers")
                <*> (fromMaybe 10 <$> obj .:? "max-db-connections")
