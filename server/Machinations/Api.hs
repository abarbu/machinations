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
import Machinations.Misc

instance ToSchema StateFormula where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Limits where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "limits"))
instance ToSchema Formula where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Waiting where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "_waiting"))
instance ToSchema AnyLabel where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptionsSingle)
instance ToSchema TransferType where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema DistributionType where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "deterministic"))
instance ToSchema PushAction where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Interval where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Overflow where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema ResourceFormula where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema PushPullAction where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema StateEdge where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema PullAction where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema ResourceEdge where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema NodeActivation where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema NodeType where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Resource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "resource"))
instance ToSchema Node where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Graph where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "graph"))
instance ToSchema StateEdgeModifiers where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
deriving newtype instance ToSchema NodeLabel
deriving newtype instance ToSchema ResourceEdgeLabel
deriving newtype instance ToSchema StateEdgeLabel
instance ToSchema Condition where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mjsonOptions)
instance ToSchema Machination where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "Machination"))
instance ToSchema RunMachination where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "runMachination"))
instance ToSchema RunResult where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (prefixOptions "runResult"))

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
