{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, TypeFamilies, GADTs, StandaloneDeriving, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DisambiguateRecordFields, DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, TypeOperators, NoMonomorphismRestriction #-}
module Machinations.Database where
import Data.Text (Text)
import Data.Time.Clock
import Data.Maybe
import Data.ByteString(ByteString)
import GHC.Records (getField)
import qualified Data.UUID as U
import Control.Monad.Extra
import qualified Database.Persist as P
import qualified Database.Persist.TH as P

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
User
    email Text
    username Text
    passwordHash ByteString
    UniqueUsername username
    UniqueEmail email
|]
