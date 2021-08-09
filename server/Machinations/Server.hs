{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs, StandaloneDeriving, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DisambiguateRecordFields, DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, TypeOperators, RecordWildCards #-}
module Machinations.Server where
import qualified Data.Text as T
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Extra
import Servant
import qualified Data.Text.Encoding as T
import qualified Machinations.Api as A
import qualified Machinations.Types as T
import qualified Machinations.Database as D
import qualified Machinations.JWT as J
import Crypto.BCrypt
import GHC.Records (getField)
import qualified Data.UUID.V4 as U
import Data.Maybe
import qualified Machinations.Types as M
import qualified Machinations.Rendering as M
import qualified Machinations as M
import Data.Set(Set)
import qualified Data.Set as S
import Servant.Swagger.UI
import Data.OpenApi               hiding (Server)

jwtTokenValiditySeconds = 31556952 :: NominalDiffTime -- 1 year

data AppConfig = AppConfig { appPool :: ConnectionPool
                           , appPort :: Int
                           , appSecret :: T.Text }

newtype App a = App { unApp :: ReaderT AppConfig (LoggingT (ExceptT ServerError IO)) a }
   deriving newtype (Applicative, Functor, Monad, MonadIO, MonadLogger,
                     MonadReader AppConfig, MonadError ServerError)

appToHandler :: AppConfig -> App a -> Handler a
appToHandler cfg app =
  Handler $ runStdoutLoggingT (runReaderT (unApp app) cfg)

runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) b -> App b
runDB q = do p <- asks appPool
             liftIO $ runSqlPersistMPool q p

throw422 e = throwError $ err422 { errBody = e }
throw401 e = throwError $ err401 { errBody = e }

serveTest :: App T.Text
serveTest = pure "Hi"

serveRender :: M.Machination -> App T.Text
serveRender = pure . T.pack . show . M.toGraph

serveRun :: M.RunMachination -> App M.RunResult
serveRun M.RunMachination{..} = pure $ M.runToResult $ M.run runMachinationMachine runMachinationActiveNodes

server :: ServerT A.API App
server = swaggerSchemaUIServerT A.swaggerDoc :<|> (serveTest :<|> serveRender :<|> serveRun)
