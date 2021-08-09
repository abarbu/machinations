{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs, StandaloneDeriving, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DisambiguateRecordFields, DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, TypeOperators #-}

module Machinations.Api where
import Data.Text (Text)
import Servant
import Servant.Server.Experimental.Auth
import Data.Aeson
import GHC.Generics
import Machinations.Types
import Data.OpenApi               hiding (Server)
import Servant.OpenApi
import Servant.Swagger.UI
import Servant.Swagger.UI.Core
import Control.Lens

instance (ToSchema StateFormula)
instance (ToSchema Limits)
instance (ToSchema Formula)
instance (ToSchema Waiting)
instance (ToSchema AnyLabel)
instance (ToSchema TransferType)
instance (ToSchema DistributionType)
instance (ToSchema PushAction)
instance (ToSchema Interval)
instance (ToSchema Overflow)
instance ToSchema ResourceFormula
instance ToSchema PushPullAction
instance ToSchema StateEdge
instance ToSchema PullAction
instance ToSchema ResourceEdge
instance ToSchema NodeActivation
instance ToSchema NodeType
instance ToSchema Resource
instance ToSchema Node
instance ToSchema Graph
deriving newtype instance ToSchema NodeLabel
deriving newtype instance ToSchema ResourceEdgeLabel
deriving newtype instance ToSchema StateEdgeLabel
instance ToSchema Condition
instance ToSchema Machination
instance ToSchema RunMachination
instance ToSchema RunResult

type RawAPI = "api" :> (("test"  :> Get '[JSON] Text)
                         :<|> ("render" :> ReqBody '[JSON] Machination :> Post '[JSON] Text )
                         :<|> ("run" :> ReqBody '[JSON] RunMachination :> Post '[JSON] RunResult ))

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"
           :<|> RawAPI

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy :: Proxy RawAPI)
    & info.title       .~ "Machinations API"
    & info.version     .~ "2021"
    & info.description ?~ "Run and visualize machinations"
