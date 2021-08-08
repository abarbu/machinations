{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings, OverloadedLabels, MagicHash, ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields, DataKinds, KindSignatures, GADTs, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, TypeFamilies, TypeOperators, PolyKinds, FlexibleContexts, ViewPatterns #-}

module Machinations.Xml where
import Machinations.Types
import Machinations.Misc
import Machinations.Formulas (parseRF, parseSF, parseF)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens hiding (from,to)
import Control.Lens.TH
import Control.Lens.Extras
import Data.String
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import Data.List (foldl', nub, nubBy, partition, groupBy)
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad
import Text.XML.JSON.XmlToJson
import Data.Aeson.Types
import qualified Data.Vector as V
import System.IO.Silently
import System.IO.Unsafe
import Control.Applicative
import Text.Read(readMaybe)
import GHC.Stack(HasCallStack)
import Debug.Trace

conv = do
  f <- capture_ (xmlToJson [] ["/tmp/a.xml"])
  writeFile "/tmp/b.json" f

read' :: (HasCallStack, Read a) => String -> String -> a
read' s i = case readMaybe i of
              Nothing -> error $ "Failed to read " <> s <> " '" <> i <> "'"
              Just x -> x

parseActivation :: Text -> NodeActivation
parseActivation "passive" = Passive
parseActivation "interactive" = Interactive
parseActivation "automatic" = Automatic
parseActivation "onstart" = OnStart -- TODO Not verified
 
parseOverflow :: Text -> Overflow
parseOverflow "block" = OverflowBlock
parseOverflow "drain" = OverflowDrain

parsePushPullAction :: Text -> PushPullAction
parsePushPullAction "push-any" = Pushing PushAny
parsePushPullAction "push-all" = Pushing PushAll
parsePushPullAction "pull-any" = Pulling PullAny
parsePushPullAction "pull-all" = Pulling PullAll

parsePullAction :: Text -> PullAction
parsePullAction "pull-any" = PullAny
parsePullAction "pull-all" = PullAll

parsePushAction :: Text -> PushAction
parsePushAction "push-any" = PushAny
parsePushAction "push-all" = PushAll

parseDistribution :: Text -> DistributionType
parseDistribution "deterministic" = Deterministic Nothing Nothing
parseDistribution "dice" = Random

parseOptionalInt (-1) = Nothing
parseOptionalInt n = Just n

parseOneOrMode :: HasCallStack => Text -> (Object -> Parser (l, o)) -> Object -> Parser [(l, o)]
parseOneOrMode label parser root = do
  (pools :: Maybe Value) <- root .:? label
  case pools of
      Just ps -> withArray "nodes"
                (traverse (withObject "node" parser) . V.toList)
                ps
                <|>
                (do
                    x <- withObject "node" parser ps
                    pure [x])
      Nothing -> pure []

parsePool :: HasCallStack => Object -> Parser (NodeLabel, Node)
parsePool obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  ppa <- obj .: "actionMode"
  amount <- obj .: "number"
  res :: Object <- obj .: "Resource"
  resTag :: ResourceTag <- res .: "name"
  o <- obj .: "overflow"
  c <- obj .: "capacity"
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Pool { _activation = parseActivation a
                        , _pushPullAction = parsePushPullAction ppa
                        , _resources =
                            S.fromList
                            $ unsafePerformIO
                            $ mapM (\_ -> Resource resTag <$> mkUuid) [1..read' "" amount]
                        , _overflow = parseOverflow o
                        , _limit = parseOptionalInt $ read' "" c }
                 , nodeLabel = fromMaybe "" l
                 , nodeColor = "black"
                 })

parseSource :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseSource obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  res :: Object <- obj .: "Resource"
  resTag :: ResourceTag <- res .: "name"
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Source { _activation = parseActivation a
                          , _resourceTypes = [resTag]
                          }
                 , nodeLabel = fromMaybe "" l
                 , nodeColor = "black"
                 })

parseDrain :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseDrain obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pa <- obj .: "actionMode"
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Drain { _activation = parseActivation a
                         , _pullAction = parsePullAction pa
                         }
                 , nodeLabel = fromMaybe "" l
                 , nodeColor = "black"
                 })

parseConverter :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseConverter obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pa <- obj .: "actionMode"
  res :: Object <- obj .: "Resource"
  resTag :: ResourceTag <- res .: "name"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Converter { _activation = parseActivation a
                            , _pullAction = parsePullAction pa
                            , _resourceTypes = [resTag]
                            , _storage = []
                            }
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

parseEndCondition :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseEndCondition obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy = EndCondition
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

parseGate :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseGate obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pa <- obj .: "actionMode"
  dm <- obj .: "distributionMode"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Gate { _activation = parseActivation a
                       , _pullAction = parsePullAction pa
                       , _distribution = parseDistribution dm
                       }
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

parseTrader :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseTrader obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Trader { _activation = parseActivation a }
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

parseDelayOrQueue :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseDelayOrQueue obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  q <- obj .: "queue"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  case q :: Text of
                    "0" -> Delay { _activation = parseActivation a, _waitingResources = [] }
                    "1" -> Queue { _activation = parseActivation a, _waitingResources = [] }
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

-- TODO I can't see any way to work around this hack, hope no one wants 9999 as a limit!
convertLimits min max = Limits (oneLimit min) (oneLimit max)
  where oneLimit "9999" = Nothing
        oneLimit "-1" = Nothing
        oneLimit n = Just $ read' "" n

-- TODO
parseFormula = maybe (FConstant 0) (fromJust . parseF)

parseRegister :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseRegister obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  isIn <- obj .: "isInteractive"
  init <- obj .: "initialValue"
  step <- obj .: "step"
  max <- obj .: "maxValue"
  min <- obj .: "minValue"
  formula :: Maybe Text <- obj .:? "formula"
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  case isIn :: Text of
                    "1" -> RegisterInteractive { _initial = read' "" init
                                              , _step = read' "" step
                                              , _currentValue = read' "" init
                                              , _limits = convertLimits max min }
                    "0" -> RegisterFn { _registerFormula = parseFormula (formula <|> l)
                                     , _limits = convertLimits max min }
              , nodeLabel = fromMaybe "" l
              , nodeColor = "black"
              })

parseResourceFormula :: Maybe Text -> ResourceFormula
parseResourceFormula = maybe (RFConstant 1) (fromJust . parseRF)

parseResource :: HasCallStack => Object -> Parser (ResourceEdgeLabel, ResourceEdge)
parseResource obj = do
  i <- obj .: "id"
  t <- obj .:? "target"
  s <- obj .:? "source"
  value <- obj .:? "value"
  formula <- obj .:? "formula"
  interval <- obj .:? "interval"
  -- resource filter
  res :: Object <- obj .: "Resource"
  resTag :: ResourceTag <- res .: "name"
  colorCoding <- obj .: "colorCoding"
  --
  transfer <- obj .: "resourceTransfer"
  shuffle <- obj .: "shuffleSource"
  pure (ResourceEdgeLabel $ read' "" i
       , ResourceEdge
         -- TODO Unconnected edges are possible in Machinations but we don't allow them by construction
         { _from = NodeLabel $ maybe 0 (read' "") s
         , _to = NodeLabel $ maybe 0 (read' "") t
         , _resourceFormula = parseResourceFormula (T.takeWhile (/='|') <$> (formula <|> value))
         , _interval = Interval (RFConstant $ maybe 1 (read' "") interval) 0
         , _transfer = case transfer :: Text of
                         "interval-based" -> IntervalTransfer
                         _ -> InstantTransfer
         , _shuffleOrigin = (shuffle :: Text) == "1"
         , _resourceFilter = if (colorCoding :: Text) == "1" then
                               Just resTag else
                               Nothing
                             -- TODO limits
         , _limits = Limits Nothing Nothing
         })

parseStateFormula x = x >>= parseSF

parseState :: HasCallStack => Object -> Parser (StateEdgeLabel, StateEdge)
parseState obj = do
  i <- obj .: "id"
  t <- obj .:? "target"
  s <- obj .:? "source"
  value <- obj .:? "value"
  formula <- obj .:? "formula"
  res :: Object <- obj .: "Resource"
  resTag :: ResourceTag <- res .: "name"
  colorCoding <- obj .: "colorCoding"
  pure (StateEdgeLabel $ read' "" i
       , StateEdge
         -- TODO Unconnected edges are possible in Machinations but we don't allow them by construction
         { _from = AnyLabel $ maybe 0 (read' "") s
         , _to = AnyLabel $ maybe 0 (read' "") t
         , _stateFormula = parseStateFormula (formula <|> value)
         , _resourceFilter =
             -- Yes, this is where they store the filter
             case colorCoding :: Text of
               "0" -> Nothing
               "1" -> Just resTag
         , _active = False
         })

parsePools = parseOneOrMode "mxPoolShapeCell" parsePool
parseSources = parseOneOrMode "mxSourceShapeCell" parseSource
parseDrains = parseOneOrMode "mxDrainShapeCell" parseDrain
parseConverters = parseOneOrMode "mxConverterShapeCell" parseConverter
parseTraders = parseOneOrMode "mxTraderShapeCell" parseTrader
parseEndConditions = parseOneOrMode "mxEndConditionShapeCell" parseEndCondition
parseGates = parseOneOrMode "mxGateShapeCell" parseGate
parseDelayOrQueues = parseOneOrMode "mxDelayShapeCell" parseDelayOrQueue
parseRegisters = parseOneOrMode "mxMachinationRegisterCell" parseRegister

parseResourceEdges = parseOneOrMode "mxResourceConnectionCell" parseResource
parseStateEdges = parseOneOrMode "mxStateConnectionCell" parseState

readMachinationsXml fname = do
  f <- capture_ (xmlToJson [] [fname])
  let (Just result) :: Maybe Object = decode (B.pack f)
  -- print result
  pure $ flip parseMaybe result $ \obj -> do
    (g :: Object) <- obj .: "mxGraphModel"
    (root :: Object) <- g .: "root"
    as <- parsePools root
    ss <- parseSources root
    ds <- parseDrains root
    cs <- parseConverters root
    ts <- parseTraders root
    es <- parseEndConditions root
    gs <- parseGates root
    qs <- parseDelayOrQueues root
    rs <- parseRegisters root
    res <- parseResourceEdges root
    sts <- parseStateEdges root
    pure $ Machination
      { machinationGraph =
        Graph { graphVertices = M.fromList $ as <> ss <> ds <> cs <> ts <> es <> qs <> rs <> gs
              , graphResourceEdges = M.fromList res
              , graphStateEdges = M.fromList sts
              }
      , machinationResourceTagColor = M.empty
      , machinationTime = 0
      , machinationSeed = 0
      }
