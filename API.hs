module API where

import ClassyPrelude
import Control.Monad.Except (ExceptT)
import Data.Aeson (ToJSON(toJSON), Value, object, (.=))
import Network.HTTP.Types (Status)

data RT where
  PingRT :: RT

data Request (rt :: RT) where
  PingRequest :: Request 'PingRT

data ResponseFor (rt :: RT) where
  PingResponse :: ResponseFor 'PingRT

type Result m a = ExceptT (Status, ErrorReason) m a

data SomeRequest = forall a . SomeRequest { unSomeRequest :: Request a }

responseToJSON :: Request a -> ResponseFor a -> Value
responseToJSON request = case request of
  PingRequest -> toJSON

instance ToJSON (ResponseFor a) where
  toJSON = \ case
    PingResponse -> object []

newtype ErrorReason = ErrorReason { unErrorReason :: Text }
instance ToJSON ErrorReason where
  toJSON (ErrorReason reason) = object ["message" .= reason]

deriving instance Eq (Request a)
deriving instance Ord (Request a)
deriving instance Show (Request a)
