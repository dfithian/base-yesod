module Ping where

import qualified API
import ClassyPrelude hiding (Handler)
import Foundation (Handler, dispatchRequest)
import Yesod (sendStatusJSON)

postPingR :: Handler ()
postPingR = do
  uncurry sendStatusJSON =<< dispatchRequest API.PingRequest
