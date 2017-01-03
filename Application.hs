{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool)
import Dispatcher (runDispatcher)
import Foundation
  ( App(App, appConnectionPool, appLog, appRequestQueue, appSettings)
  , resourcesApp, Route(PingR) )
import Ping (postPingR)
import Settings (appMaxDbConnections, appMaxWorkers)
import Yesod (defaultMakeLogger, mkYesodDispatch, warp)
import Yesod.Core.Types (loggerPutStr)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

mkYesodDispatch "App" resourcesApp

makeFoundation :: IO App
makeFoundation = do
  appSettings <- loadYamlSettings ["settings.yml"] [] useEnv

  appRequestQueue <- newTBChanIO $ appMaxWorkers appSettings

  appLog <- (\ logger _ _ _ str -> loggerPutStr logger str) <$> defaultMakeLogger

  appConnectionPool <- flip runLoggingT appLog $
    createPostgresqlPool "authentication-db" (appMaxDbConnections appSettings)

  pure $ App {..}

startApplication :: IO ()
startApplication = do
  appl <- makeFoundation

  runLoggingT (runReaderT runDispatcher appl) (appLog appl)

  warp 3000 appl
