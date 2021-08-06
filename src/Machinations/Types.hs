{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, Strict, StrictData #-}

module Machinations.Types where
import Data.Aeson
import Data.Aeson.TH
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens
import Control.Lens.TH
import GHC.Generics
import Machinations.Misc
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Data.UUID

type Variable = Text
type ResourceTag = Text
type Filter = ResourceTag
type GraphLabel = Int
type Probability = Float
type Percentage = Float

data Resource = Resource { resourceTag :: ResourceTag,
                           resourceUUID :: Text }
  deriving (Show, Eq)
deriveJSON (prefixOptions "resource") ''Resource
makeFields ''Resource

instance Ord Resource where
  x `compare` x' = resourceUUID x `compare` resourceUUID x'

data Condition = CEqual
               | CNotEqual
               | CGt
               | CLt
               | CGtEq
               | CLtEq
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Condition
makePrisms ''Condition

data ResourceFormula = RFAll
                     | RFMultiply ResourceFormula ResourceFormula
                     | RFDivide ResourceFormula ResourceFormula
                     | RFAdd ResourceFormula ResourceFormula
                     | RFSubtract ResourceFormula ResourceFormula
                     | RFNegation ResourceFormula
                     | RFPercentage ResourceFormula
                     | RFDice ResourceFormula ResourceFormula
                     | RFConstant Int
                     | RFCondition Condition ResourceFormula
  deriving (Show, Eq)
deriveJSON mjsonOptions ''ResourceFormula
makePrisms ''ResourceFormula

data Interval = Interval { intervalFormula :: ResourceFormula
                         , intervalCounter :: Int }
  deriving (Show, Eq)
deriveJSON (prefixOptions "interval") ''Interval
makeFields ''Interval

data Limits = Limits { limitsLower :: Maybe Int
                     , limitsUpper :: Maybe Int }
  deriving (Show, Eq)
deriveJSON (prefixOptions "limits") ''Limits
makeFields ''Limits

data Overflow = OverflowBlock | OverflowDrain
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Overflow
makePrisms ''Overflow

data PushAction = PushAny | PushAll
  deriving (Show, Eq)
deriveJSON mjsonOptions ''PushAction
makePrisms ''PushAction

data PullAction = PullAny | PullAll
  deriving (Show, Eq)
deriveJSON mjsonOptions ''PullAction
makePrisms ''PullAction

data PushPullAction = Pushing PushAction | Pulling PullAction
  deriving (Show, Eq)
deriveJSON mjsonOptions ''PushPullAction
makePrisms ''PushPullAction

-- NB This is not a "distribution"
data DistributionType = Deterministic { _counts :: Maybe (Map GraphLabel Int, Int)
                                      , _lastNode :: Maybe Int }
                      | Random
  deriving (Show, Eq)
deriveJSON (prefixOptions "deterministic") ''DistributionType
makePrisms ''DistributionType
makeFieldsNoPrefix ''DistributionType

data NodeActivation = Passive
                    | Interactive
                    | Automatic
                    | OnStart
  deriving (Show, Eq)
deriveJSON mjsonOptions ''NodeActivation
makePrisms ''NodeActivation

data TransferType = IntervalTransfer | InstantTransfer
  deriving (Show, Eq)
deriveJSON mjsonOptions ''TransferType
makePrisms ''TransferType

-- * Formulas

-- https://machinations.io/docs/registers/math-js-functions/
data Formula = FVar Variable
             | FConstant Int
             | FApply Formula Formula
             | FPair Formula Formula
             -- Math
             | FNeg Formula
             | FAdd Formula Formula
             | FSub Formula Formula
             | FMul Formula Formula
             | FDiv Formula Formula
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Formula
makePrisms ''Formula

data Dice = Dice { diceNr :: Int
                 , diceSide :: Int }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Dice
makeFields ''Dice

data RandomRate = RandomDice { randomRateDice :: Dice }
                | RandomChance Probability
                | RandomPercentage Percentage
  deriving (Show, Eq)
deriveJSON mjsonOptions ''RandomRate
makePrisms ''RandomRate

data Range = Range { rangeLower :: Int
                   , rangeUpper :: Int }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Range
makeFields ''Range

-- data Modifier = Plus Int
--               | PlusProbability Probability
--               | PlusInterval Int
--   deriving (Show, Eq)
-- deriveJSON mjsonOptions ''Modifier
-- makePrisms ''Modifier

data StateFormula = SFAdd StateFormula
                  | SFSub StateFormula
                  | SFMul StateFormula
                  | SFDiv StateFormula
                  | SFCondition Condition StateFormula
                  | SFOverwrite StateFormula
                  | SFPercentage StateFormula
                  | SFRange StateFormula StateFormula
                  | SFInterval StateFormula
                  | SFConstant Int
                  | SFTrigger
                  | SFReverseTrigger
                  | SFVariable Text
  deriving (Show, Eq)
deriveJSON mjsonOptions ''StateFormula
makePrisms ''StateFormula

data Waiting = Waiting { _waitingStartTime :: Int
                       , _waitingAmount :: Int
                       , _waitingResource :: Resource }
  deriving (Show, Eq)
deriveJSON (prefixOptions "_waiting") ''Waiting
makeFields ''Waiting

data NodeType = Source { _activation :: NodeActivation
                       , _resourceTypes :: Set ResourceTag
                       }
              | Drain { _activation :: NodeActivation
                      , _pullAction :: PullAction
                      }
              | Pool { _activation :: NodeActivation
                     , _pushPullAction :: PushPullAction
                     , _resources :: Set Resource
                     , _overflow :: Overflow
                     , _limit :: Maybe Int
                     }
              | Gate { _activation :: NodeActivation
                     , _pullAction :: PullAction
                     , _distribution :: DistributionType
                     }
              | Trader { _activation :: NodeActivation
                       }
              | Converter { _activation :: NodeActivation
                          , _pullAction :: PullAction
                          , _resourceTypes :: Set ResourceTag
                          }
              | RegisterFn { _registerFormula :: Formula
                           , _limits :: Limits }
              | RegisterInteractive { _initial :: Int
                                    , _step :: Int
                                    , _currentValue :: Int
                                    , _limits :: Limits}
              | Delay { _activation :: NodeActivation
                      , _waitingResources :: [Waiting] }
              | Queue { _activation :: NodeActivation
                      , _waitingResources :: [Waiting] }
              | EndCondition
  deriving (Show, Eq)
deriveJSON mjsonOptions ''NodeType
makePrisms ''NodeType
makeFieldsNoPrefix ''NodeType

data Node = Node { nodeTy :: NodeType
                 , nodeLabel :: Text
                 , nodeColor :: Text
                 }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''Node
makeFields ''Node

data ResourceEdge = ResourceEdge { _from :: GraphLabel
                                 , _to :: GraphLabel
                                 , _resourceFormula :: ResourceFormula
                                 , _interval :: Interval
                                 , _transfer :: TransferType
                                 , _resourceFilter :: Maybe Filter
                                 , _shuffleOrigin :: Bool
                                 , _limits :: Limits
                                 }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''ResourceEdge
makeFieldsNoPrefix ''ResourceEdge

-- types of state edges:
--
-- label modifiers
-- node modifiers
-- triggers
-- activators

data StateEdge = StateEdge { _from :: GraphLabel
                           , _to :: GraphLabel
                           , _stateFormula :: Maybe StateFormula
                           , _resourceFilter :: Maybe Filter
                           , _active :: Bool
                           }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''StateEdge
makeFieldsNoPrefix ''StateEdge

data Graph = Graph { graphVertices :: Map GraphLabel Node
                   , graphResourceEdges :: Map GraphLabel ResourceEdge
                   , graphStateEdges :: Map GraphLabel StateEdge
                   }
  deriving (Show, Eq)
deriveJSON (prefixOptions "graph") ''Graph
makeFields ''Graph

data Machination = Machination { machinationGraph :: Graph
                               , machinationResourceTagColor :: Map ResourceTag Text
                               , machinationTime :: Int
                               , machinationSeed :: Int
                               }
  deriving (Show, Eq)
deriveJSON (prefixOptions "Machination") ''Machination
makeFields ''Machination

data NodeOrEdge = N (GraphLabel, Node)
                | R (GraphLabel, ResourceEdge)
                | S (GraphLabel, StateEdge)
makePrisms ''NodeOrEdge

instance Eq NodeOrEdge where
  x == x' = toLabel x == toLabel x'

instance Ord NodeOrEdge where
  x `compare` x' = toLabel x `compare` toLabel x'

toLabel :: NodeOrEdge -> GraphLabel
toLabel (N (l, _)) = l
toLabel (R (l, _)) = l
toLabel (S (l, _)) = l

data RunResult = RunResult { runResultUpdate :: Machination
                           , runResultActivated :: GraphLabel
                           , runResultFailed :: GraphLabel
                           }
  deriving (Show, Eq)
deriveJSON mjsonOptions ''RunResult
makeFields ''RunResult

isPool :: NodeType -> Bool
isPool Pool{} = True
isPool _ = False

isGate :: NodeType -> Bool
isGate Gate{} = True
isGate _ = False

isLatched :: NodeType -> Bool
isLatched Source{} = True
isLatched Drain{} = True
isLatched Pool{} = True
isLatched _ = False
