{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, Strict, StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Text.PrettyPrint as P
import System.Random(mkStdGen,random,randomR,StdGen)

type Variable = Text
type ResourceTag = Text
type Filter = ResourceTag
type Probability = Float
type Percentage = Float

newtype NodeLabel = NodeLabel { unNodeLabel :: Int }
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
makePrisms ''NodeLabel

newtype ResourceEdgeLabel = ResourceEdgeLabel { unResourceEdgeLabel :: Int }
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
makePrisms ''ResourceEdgeLabel

newtype StateEdgeLabel = StateEdgeLabel { unStateEdgeLabel :: Int }
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
makePrisms ''StateEdgeLabel

newtype AnyLabel = AnyLabel { unAnyLabel :: Int }
  deriving (Show, Eq, Generic, Ord)
deriveJSON mjsonOptionsSingle ''AnyLabel
makePrisms ''AnyLabel

class ToAnyLabel a where
  toAnyLabel :: a -> AnyLabel
instance ToAnyLabel NodeLabel where
  toAnyLabel (NodeLabel l) = AnyLabel l
instance ToAnyLabel ResourceEdgeLabel where
  toAnyLabel (ResourceEdgeLabel l) = AnyLabel l
instance ToAnyLabel StateEdgeLabel where
  toAnyLabel (StateEdgeLabel l) = AnyLabel l

toNodeLabel (AnyLabel l) = NodeLabel l
toResourceEdgeLabel (AnyLabel l) = ResourceEdgeLabel l
toStateEdgeLabel (AnyLabel l) = StateEdgeLabel l

data Resource = Resource { resourceTag :: ResourceTag,
                           resourceUUID :: Text }
  deriving (Show, Eq, Generic)
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
  deriving (Show, Eq, Ord, Generic)
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
  deriving (Show, Eq, Generic, Ord)
deriveJSON mjsonOptions ''ResourceFormula
makePrisms ''ResourceFormula

data Interval = Interval { intervalFormula :: ResourceFormula
                         , intervalCounter :: Int }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "interval") ''Interval
makeFields ''Interval

data Limits = Limits { limitsLower :: Maybe Int
                     , limitsUpper :: Maybe Int }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "limits") ''Limits
makeFields ''Limits

data Overflow = OverflowBlock | OverflowDrain
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''Overflow
makePrisms ''Overflow

data PushAction = PushAny | PushAll
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''PushAction
makePrisms ''PushAction

data PullAction = PullAny | PullAll
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''PullAction
makePrisms ''PullAction

data PushPullAction = Pushing PushAction | Pulling PullAction
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''PushPullAction
makePrisms ''PushPullAction

-- NB This is not a "distribution"
data DistributionType = Deterministic { _counts :: Maybe (Map ResourceEdgeLabel Int, Int)
                                      , _lastEdge :: Maybe ResourceEdgeLabel }
                      | Random
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "deterministic") ''DistributionType
makePrisms ''DistributionType
makeFieldsNoPrefix ''DistributionType

data NodeActivation = Passive
                    | Interactive
                    | Automatic
                    | OnStart
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''NodeActivation
makePrisms ''NodeActivation

data TransferType = IntervalTransfer | InstantTransfer
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''TransferType
makePrisms ''TransferType

-- * Formulas

-- https://machinations.io/docs/registers/math-js-functions/
data Formula = FVar Variable
             | FConstant Double
             | FApply Formula Formula
             | FPair Formula Formula
             -- Math
             | FNeg Formula
             | FAdd Formula Formula
             | FSub Formula Formula
             | FMul Formula Formula
             | FDiv Formula Formula
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''Formula
makePrisms ''Formula

data Dice = Dice { diceNr :: Int
                 , diceSide :: Int }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''Dice
makeFields ''Dice

data RandomRate = RandomDice { randomRateDice :: Dice }
                | RandomChance Probability
                | RandomPercentage Percentage
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''RandomRate
makePrisms ''RandomRate

data Range = Range { rangeLower :: Int
                   , rangeUpper :: Int }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''Range
makeFields ''Range

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
  deriving (Show, Eq, Generic, Ord)
deriveJSON mjsonOptions ''StateFormula
makePrisms ''StateFormula

data Waiting = Waiting { _waitingStartTime :: Int
                       , _waitingResource :: Resource }
  deriving (Show, Eq, Generic)
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
                          , _storage :: Map ResourceEdgeLabel (Set Resource)
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
                      , _waitingResources :: [Waiting]
                      , _nextTimeAvailable :: Maybe Int }
              | EndCondition
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''NodeType
makePrisms ''NodeType
makeFieldsNoPrefix ''NodeType

data Node = Node { nodeTy :: NodeType
                 , nodeLabel :: Text
                 , nodeColor :: Text
                 }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''Node
makeFields ''Node

data ResourceEdge = ResourceEdge { _from :: NodeLabel
                                 , _to :: NodeLabel
                                 , _resourceFormula :: ResourceFormula
                                 , _interval :: Interval
                                 , _transfer :: TransferType
                                 , _resourceFilter :: Maybe Filter
                                 , _shuffleOrigin :: Bool
                                 , _limits :: Limits
                                 }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''ResourceEdge
makeFieldsNoPrefix ''ResourceEdge

-- types of state edges:
--
-- label modifiers
-- node modifiers
-- triggers
-- activators

data StateEdge = StateEdge { _from :: AnyLabel
                           , _to :: AnyLabel
                           , _stateFormula :: Maybe StateFormula
                           , _resourceFilter :: Maybe Filter
                           , _active :: Bool
                           }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''StateEdge
makeFieldsNoPrefix ''StateEdge

data Graph = Graph { graphVertices :: Map NodeLabel Node
                   , graphResourceEdges :: Map ResourceEdgeLabel ResourceEdge
                   , graphStateEdges :: Map StateEdgeLabel StateEdge
                   }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "graph") ''Graph
makeFields ''Graph

data Machination = Machination { machinationGraph :: Graph
                               , machinationResourceTagColor :: Map ResourceTag Text
                               , machinationTime :: Int
                               , machinationSeed :: Int
                               , machinationPendingTriggers :: Set NodeLabel
                               }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "Machination") ''Machination
makeFields ''Machination

data ResolvedLabel = RNode NodeLabel Node
                   | RResource ResourceEdgeLabel ResourceEdge
                   | RState StateEdgeLabel StateEdge
                   deriving (Show, Eq)
 
isPool :: Node -> Bool
isPool Node {nodeTy = Pool{}} = True
isPool _ = False

isGate :: Node -> Bool
isGate Node {nodeTy = Gate{}} = True
isGate _ = False

isTrader :: Node -> Bool
isTrader Node {nodeTy = Trader{}} = True
isTrader _ = False

isConverter :: Node -> Bool
isConverter Node {nodeTy = Converter{}} = True
isConverter _ = False

isLatched :: Node -> Bool
isLatched Node {nodeTy = Source{}} = True
isLatched Node {nodeTy = Drain{}} = True
isLatched Node {nodeTy = Pool{}} = True
isLatched _ = False

isEndCondition :: Node -> Bool
isEndCondition Node {nodeTy = EndCondition{}} = True
isEndCondition _ = False

isRegisterFn :: Node -> Bool
isRegisterFn Node {nodeTy = RegisterFn{}} = True
isRegisterFn _ = False

isRegisterInteractive :: Node -> Bool
isRegisterInteractive Node {nodeTy = RegisterInteractive{}} = True
isRegisterInteractive _ = False

isAnyRegister :: Node -> Bool
isAnyRegister r = isRegisterInteractive r || isRegisterFn r

data StateEdgeModifiers =
  StateEdgeModifiers { _triggerNode :: Set NodeLabel
                     , _enableNode :: Set NodeLabel
                     , _disableNode :: Set NodeLabel
                     , _enableResourceEdge :: Set ResourceEdgeLabel
                     , _disableResourceEdge :: Set ResourceEdgeLabel
                     , _modifyResourceFormula :: Map ResourceEdgeLabel (Set StateFormula)
                     , _modifyNode :: Map NodeLabel (Set StateFormula) }
  deriving (Show, Eq, Generic)
deriveJSON mjsonOptions ''StateEdgeModifiers
makeFieldsNoPrefix ''StateEdgeModifiers

data Run = Run { runOldUpdate          :: Machination
               , runNewUpdate          :: Machination
               , runRegisterValues     :: Map NodeLabel Double
               , runStateEdgeModifiers :: StateEdgeModifiers
               -- TODO Verify these
               , runActivatedEdges     :: Set ResourceEdgeLabel
               , runFailedEdges        :: Set ResourceEdgeLabel
               , runActivatedNodes     :: Set NodeLabel
               , runFailedNodes        :: Set NodeLabel
               , runTriggeredEdges     :: Set StateEdgeLabel
               , runEdgeflow           :: Map ResourceEdgeLabel (Set Resource)
               , runNodeInflow         :: Map NodeLabel (Set Resource)
               , runNodeOutflow        :: Map NodeLabel (Set Resource)
               -- NB Checked against the model
               , runGeneratedResources :: Set Resource
               -- NB Checked against the model
               , runKilledResources    :: Set Resource
               , runStdGen             :: StdGen
               , runErrors             :: Map AnyLabel Text
               , runEnded              :: Maybe NodeLabel
               }
  deriving (Show, Eq, Generic)
makeFields ''Run

data RunMachination = RunMachination { runMachinationMachine         :: Machination
                                     , runMachinationActiveNodes     :: Set NodeLabel
                                     }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "runMachination") ''RunMachination
makeFields ''RunMachination

data RunResult = RunResult { runResultMachine            :: Machination
                           , runResultRegisterValues     :: Map NodeLabel Double
                           , runResultStateEdgeModifiers :: StateEdgeModifiers
                           , runResultActivatedEdges     :: Set ResourceEdgeLabel
                           , runResultFailedEdges        :: Set ResourceEdgeLabel
                           , runResultActivatedNodes     :: Set NodeLabel
                           , runResultFailedNodes        :: Set NodeLabel
                           , runResultTriggeredEdges     :: Set StateEdgeLabel
                           , runResultEdgeflow           :: Map ResourceEdgeLabel (Set Resource)
                           , runResultGeneratedResources :: Set Resource
                           , runResultKilledResources    :: Set Resource
                           , runResultErrors             :: Map Int Text
                           , runResultEnded              :: Maybe NodeLabel
                           }
  deriving (Show, Eq, Generic)
deriveJSON (prefixOptions "runResult") ''RunResult
makeFields ''RunResult

mkStateEdgeModifiers :: StateEdgeModifiers
mkStateEdgeModifiers = StateEdgeModifiers S.empty S.empty S.empty S.empty S.empty M.empty M.empty

mkRun :: Machination -> Run
mkRun m = Run m m M.empty mkStateEdgeModifiers S.empty S.empty S.empty S.empty S.empty M.empty M.empty M.empty S.empty S.empty (mkStdGen $ m^.seed) M.empty Nothing

runToResult :: Run -> RunResult
runToResult Run{..} = RunResult runNewUpdate runRegisterValues runStateEdgeModifiers
                               runActivatedEdges runFailedEdges runActivatedNodes runFailedNodes
                               runTriggeredEdges runEdgeflow
                               runGeneratedResources runKilledResources
                               (M.mapKeys (\(AnyLabel l) -> l) runErrors) runEnded

summarize :: Run -> String
summarize r = P.render $
  "" P.$$ P.text "Run summary:" P.$$
  P.nest 1 (P.text "Old:" P.$$ (sm $ r^.oldUpdate)
   P.$$
   P.text "New:" P.$$ (sm $ r^.newUpdate)
   P.$$ P.text "Generated" P.<+> P.int (S.size (r^.generatedResources))
   P.$$ P.text "Killed" P.<+> P.int (S.size (r^.killedResources)))
  where sm m = P.nest 2 $ P.vcat $ map sp $ M.toList $ M.filter (\n -> isPool n) $ m^.graph . vertices
        sp (l,Node p@Pool{} _ _) = P.sizedText 6 (show l) P.<+> P.sizedText 6 "Pool" P.<+> P.sizedText 6 (show $ S.size $ _resources p)
        sp _ = ""
