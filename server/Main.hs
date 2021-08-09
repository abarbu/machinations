{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyCase, TupleSections, MultiWayIf, RecordWildCards, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings, DataKinds #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction, DerivingStrategies #-}

module Main where
import System.Environment (lookupEnv)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Server.Experimental.Auth
import Text.Read
import Data.Maybe
import qualified Machinations.Api as A
import qualified Machinations.Server as S
import qualified Machinations.Database as D
import qualified Machinations.JWT as J
import Database.Persist.Sqlite
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.Middleware.RequestLogger
import Data.Either.Combinators
import Control.Monad.IO.Class
import Network.Wai.Middleware.Cors
import Data.Time.Clock

app :: S.AppConfig -> Application
app cfg  = handleCors $ serveWithContext api authContext
           (hoistServerWithContext api
            (Proxy :: Proxy '[])
            (S.appToHandler cfg)
            S.server)
  where api = Proxy :: Proxy A.API
        s = S.appSecret cfg
        p = S.appPool cfg
        authContext = EmptyContext

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
        , corsRequestHeaders = ["Authorization", "Content-Type"] }

handleCors = cors (const $ Just corsResourcePolicy)

main = do
  port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
  secret <- fromMaybe "thisisnotsecuredontrelyonitthisisnotsecuredontrelyonitthisisnotsecuredontrelyonit"
           . (>>= readMaybe) <$> lookupEnv "SECRET"
  pool <- runStdoutLoggingT $ createSqlitePool "machinations-server.sqlite" 5
  putStrLn $ T.unpack
           $ layoutWithContext (Proxy :: Proxy A.API)
                               EmptyContext
  runSqlPersistMPool (runMigration D.migrateAll) pool
  withStdoutLogger $ \aplogger -> do
    let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
    Warp.runSettings settings
      $ logStdoutDev
      $ app
      $ S.AppConfig { S.appPool = pool,
                      S.appPort = port,
                      S.appSecret = secret }
