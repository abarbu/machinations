{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings, OverloadedLabels, MagicHash, ConstraintKinds, MultiWayIf #-}
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
import Data.List (foldl', nub, nubBy, partition, groupBy, isInfixOf)
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
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO.Temp
import Text.XML.JSON.XmlToJson
import GHC.IO.Handle

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

resTagParser obj = do
  res :: Maybe Object <- obj .:? "Resource"
  resColor :: Maybe String <- obj .:? "resourcesColor"
  resTag :: ResourceTag <- case (res, resColor) of
    (Just r, _) -> r .: "name"
    (_, Just c) -> if
      | "Black"  `isInfixOf` c -> pure "Black"
      | "Blue"   `isInfixOf` c -> pure "Blue"
      | "Green"  `isInfixOf` c -> pure "Green"
      | "Orange" `isInfixOf` c -> pure "Orange"
      | "Red"    `isInfixOf` c -> pure "Red"
  pure resTag

splitLabel Nothing = ("", Nothing)
splitLabel (Just l) =
  case T.splitOn ";" l of
    [label] ->  (label, Nothing)
    [label,events] -> (label, Just $ TriggeredByEvent (S.fromList (map Event $ T.splitOn "," events)))

parsePool :: HasCallStack => Object -> Parser (NodeLabel, Node)
parsePool obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  ppa <- obj .: "actionMode"
  amount <- obj .: "number"
  resTag <- resTagParser obj
  o <- obj .: "overflow"
  c <- obj .: "capacity"
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Pool { _activation = fromMaybe (parseActivation a) triggers
                        , _pushPullAction = parsePushPullAction ppa
                        , _resources =
                            S.fromList
                            $ unsafePerformIO
                            $ mapM (\_ -> Resource resTag <$> mkUuid) [1..read' "" amount]
                        , _overflow = parseOverflow o
                        , _limit = parseOptionalInt $ read' "" c }
                 , nodeLabel = label
                 , nodeColor = "black"
                 })

parseSource :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseSource obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  resTag <- resTagParser obj
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Source { _activation = fromMaybe (parseActivation a) triggers
                          , _resourceTypes = [resTag]
                          }
                 , nodeLabel = label
                 , nodeColor = "black"
                 })

parseDrain :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseDrain obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pa <- obj .: "actionMode"
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
        , Node { nodeTy =
                   Drain { _activation = fromMaybe (parseActivation a) triggers
                         , _pullAction = parsePullAction pa
                         }
                 , nodeLabel = label
                 , nodeColor = "black"
                 })

parseConverter :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseConverter obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  pa <- obj .: "actionMode"
  resTag <- resTagParser obj
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Converter { _activation = fromMaybe (parseActivation a) triggers
                            , _pullAction = parsePullAction pa
                            , _resourceTypes = [resTag]
                            , _storage = []
                            }
              , nodeLabel = label
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
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Gate { _activation = fromMaybe (parseActivation a) triggers
                       , _pullAction = parsePullAction pa
                       , _distribution = parseDistribution dm
                       }
              , nodeLabel = label
              , nodeColor = "black"
              })

parseTrader :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseTrader obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  Trader { _activation = fromMaybe (parseActivation a) triggers }
              , nodeLabel = label
              , nodeColor = "black"
              })

parseDelayOrQueue :: HasCallStack => Object -> Parser (NodeLabel, Node)
parseDelayOrQueue obj = do
  i <- obj .: "id"
  l <- obj .:? "value"
  a <- obj .: "activation"
  q <- obj .: "queue"
  let (label, triggers) = splitLabel l
  pure (NodeLabel $ read' "" i
       , Node { nodeTy =
                  case q :: Text of
                    "0" -> Delay { _activation = fromMaybe (parseActivation a) triggers
                                , _waitingResources = []
                                }
                    "1" -> Queue { _activation = fromMaybe (parseActivation a) triggers
                                , _waitingResources = []
                                , _nextTimeAvailable = Nothing
                                }
              , nodeLabel = label
              , nodeColor = "black"
              })

-- TODO I can't see any way to work around this hack, hope no one wants 9999 as a limit!
convertLimits min max = Limits (oneLimit min) (oneLimit max)
  where oneLimit "9999" = Nothing
        oneLimit "-9999" = Nothing
        oneLimit "-1" = Nothing
        oneLimit n = Just $ read' "" n

-- todo
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
  let (label, triggers) = splitLabel l
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

parseResourceFormula :: Maybe Text -> (ResourceFormula, Maybe ResourceConstraint)
parseResourceFormula Nothing = (RFConstant 1, Nothing)
parseResourceFormula (Just t) = (fromMaybe (RFConstant 1) rf,c)
  where (rf,c) = parseRF t

fixHtmlInXml = T.replace "&lt;" "<"
             . T.replace "&gt;" ">"
             . T.replace "<span>" ""
             . T.replace "</span>" ""

parseResource :: HasCallStack => Object -> Parser (ResourceEdgeLabel, ResourceEdge)
parseResource obj = do
  i <- obj .: "id"
  t <- obj .:? "target"
  s <- obj .:? "source"
  value <- obj .:? "value"
  formula <- obj .:? "formula"
  label <- obj .:? "label"
  interval <- obj .:? "interval"
  -- resource filter
  resTag <- resTagParser obj
  colorCoding <- obj .: "colorCoding"
  --
  transfer <- obj .: "resourceTransfer"
  shuffle <- obj .: "shuffleSource"
  let (rf,c) = parseResourceFormula (T.takeWhile (/='|') <$>
                                      (fmap fixHtmlInXml
                                       $ formula <|> value <|> label))
  pure (ResourceEdgeLabel $ read' "" i
       , ResourceEdge
         -- TODO Unconnected edges are possible in Machinations but we don't allow them by construction
         { _from = NodeLabel $ maybe 0 (read' "") s
         , _to = NodeLabel $ maybe 0 (read' "") t
         , _resourceFormula = rf
         , _interval = Interval (fst $ parseResourceFormula (case T.dropWhile (/='|') <$> interval of
                                                          Just "" -> interval
                                                          x -> x)) 0
           -- Interval (RFConstant $ maybe 1 (read' "") interval) 0
         , _transfer = case transfer :: Text of
                         "interval-based" -> IntervalTransfer
                         _ -> InstantTransfer
         , _shuffleOrigin = (shuffle :: Text) == "1"
         , _resourceFilter = if (colorCoding :: Text) == "1" then
                               Just resTag else
                               Nothing
                             -- TODO limits
         , _limits = Limits Nothing Nothing
         , _constraints = c
         })

parseStateFormula x = x >>= parseSF

parseState :: HasCallStack => Object -> Parser (StateEdgeLabel, StateEdge)
parseState obj = do
  i <- obj .: "id"
  t <- obj .:? "target"
  s <- obj .:? "source"
  value <- obj .:? "value"
  formula <- obj .:? "formula"
  label <- obj .:? "label"
  resTag <- resTagParser obj
  colorCoding <- obj .: "colorCoding"
  pure (StateEdgeLabel $ read' "" i
       , StateEdge
         -- TODO Unconnected edges are possible in Machinations but we don't allow them by construction
         { _from = AnyLabel $ maybe 0 (read' "") s
         , _to = AnyLabel $ maybe 0 (read' "") t
         , _stateFormula = parseStateFormula (fmap fixHtmlInXml
                                               $ formula <|> value <|> label)
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

convertMachinationsXmlInJSON :: FilePath -> IO ()
convertMachinationsXmlInJSON fname = do
  f <- B.readFile fname
  let (Just a :: Maybe Object) = decode f
  m <- convertXml (case a H.! "content" of
                    Object x -> case x H.! "xml" of
                                 String s -> s)
  B.writeFile (dropExtension fname <> ".json") (encodePretty m)

convertXml :: Text -> IO (Maybe Machination)
convertXml contents = do
  withSystemTempFile "convert.xml" $ \fname handle -> do
    T.hPutStr handle contents
    hClose handle
    readMachinationsXml fname

convertXmlFile :: FilePath -> IO ()
convertXmlFile fname = do
  contents <- T.readFile fname
  m <- convertXml contents
  B.writeFile (dropExtension fname <> ".json") (encodePretty m)

readMachinationsXml :: String -> IO (Maybe Machination)
readMachinationsXml fname = do
  f <- capture_ (xmlToJson [] [fname])
  let (Just result) :: Maybe Object = decode (B.pack f)
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
      , machinationPendingTriggers = S.empty
      }
