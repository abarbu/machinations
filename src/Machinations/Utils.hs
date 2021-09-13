{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyCase, TupleSections, MultiWayIf, RecordWildCards, GADTs #-}
{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fwarn-missing-signatures -Wall -Wno-name-shadowing -Wno-type-defaults #-}

module Machinations.Utils where
import Machinations.Types
import Machinations.Misc
import Machinations.Rendering
import Control.Lens hiding (from,to)
import Data.Maybe
import Data.List (groupBy, sortOn, mapAccumL)
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map.Strict(Map)
import Data.Text(Text)
import qualified Data.Map.Strict as M
import Control.Monad
import System.Random(randomR)
import System.Directory
import System.FilePath
import qualified Shelly as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Dot hiding (Graph,Node)
import Text.Printf
import Machinations.Xml
import qualified Data.Graph as G
import Control.Exception

-- | Full nodes don't count for some operations, like those affecting gates
isNodeFull :: NodeType -> Bool
isNodeFull Pool{..}  = maybe False (S.size _resources >=) _limit
-- Can never be full
isNodeFull Drain{}  = False
isNodeFull Gate{}  = False
isNodeFull Trader{}  = False
isNodeFull Converter{}  = False
isNodeFull Delay{}  = False
isNodeFull Queue{}  = False
-- These node types can never accept resources
isNodeFull Source{} = True
isNodeFull RegisterFn{}  = True
isNodeFull RegisterInteractive{}  = True
isNodeFull EndCondition{}  = True

nodeLookup :: Machination -> NodeLabel -> (NodeLabel, Node)
nodeLookup m l = (l, fromJust $ M.lookup l (m^.graph.vertices))

resourceStatsByTag :: Set Resource -> Map ResourceTag Int
resourceStatsByTag s = M.fromList $ map (\a@(h:_) -> (resourceTag h, length a))
                     $ groupBy (\x y -> resourceTag x == resourceTag y) $ sortOn resourceTag $ S.toList s

nodeResources :: Machination -> NodeLabel -> Maybe (Set Resource)
nodeResources m l = m^?graph.vertices.ix l.ty.resources

outResourceEdges :: Machination -> NodeLabel -> [(ResourceEdgeLabel, ResourceEdge)]
outResourceEdges m l = M.toList $ M.filter (\e -> e^.from == l) (m^.graph.resourceEdges)

outStateEdges :: Machination -> NodeLabel -> [(StateEdgeLabel, StateEdge)]
outStateEdges m l = M.toList $ M.filter (\e -> e^.from == toAnyLabel l) (m^.graph.stateEdges)

inResourceEdges :: Machination -> NodeLabel -> [(ResourceEdgeLabel, ResourceEdge)]
inResourceEdges m l = M.toList $ M.filter (\e -> e^.to == l) (m^.graph.resourceEdges)

inStateEdges :: Machination -> NodeLabel -> [(StateEdgeLabel, StateEdge)]
inStateEdges m l = M.toList $ M.filter (\e -> e^.to == toAnyLabel l) (m^.graph.stateEdges)

automaticNodes :: Machination -> [(NodeLabel, Node)]
automaticNodes = M.toList . M.filter (isJust . (^? (ty.activation._Automatic))) . graphVertices . machinationGraph

startNodes :: Machination -> [(NodeLabel, Node)]
startNodes = M.toList . M.filter (isJust . (^? (ty.activation._OnStart))) . graphVertices . machinationGraph

-- | If anything activates the edge, it's active, even if something else inactivates it
isActiveResourceEdge :: Run -> ResourceEdgeLabel -> Bool
isActiveResourceEdge r l
  | isJust $ e >>= \e' -> r^?stateEdgeModifiers.disableNode.ix (e'^.to) = False
  | isJust $ e >>= \e' -> r^?stateEdgeModifiers.disableNode.ix (e'^.from) = False
  | isJust $ r^?stateEdgeModifiers.enableResourceEdge.ix l = True
  | isJust $ r^?stateEdgeModifiers.disableResourceEdge.ix l = False
  | otherwise = True
  where e = r^?oldUpdate.graph.resourceEdges.ix l

-- | If anything activates the node, it's active, even if something else inactivates it
isActiveNode :: Run -> NodeLabel -> Bool
isActiveNode r l | isJust $ r^?stateEdgeModifiers.enableNode.ix l = True
                 | isJust $ r^?stateEdgeModifiers.disableNode.ix l = False
                 | otherwise = True

-- | Compute the actual value of a resource formula, this doesn't apply any state edge modifiers
-- TODO This is wonky, should separate out [all, int, percentage, and condition] but machinations doesn't!
rawResourceFormulaValue :: Run -> ResourceFormula -> (Run, Maybe Int)
rawResourceFormulaValue r RFAll = (r, Nothing)
rawResourceFormulaValue r (RFMultiply x y) = do
  let (r', x')  = rawResourceFormulaValue r x
      (r'', y') = rawResourceFormulaValue r' y
    in (r'', liftM2 (*) x' y')
rawResourceFormulaValue r (RFAdd x y) = do
  let (r', x')  = rawResourceFormulaValue r x
      (r'', y') = rawResourceFormulaValue r' y
    in (r'', liftM2 (+) x' y')
rawResourceFormulaValue r (RFConstant x) = (r, pure x)
rawResourceFormulaValue r (RFPercentage x) = rawResourceFormulaValue r x
  -- TODO These kinds of resource formulas have no value, they're a filter
rawResourceFormulaValue _ RFCondition{} = error "I don't undertand what conditions on resource edges mean"
rawResourceFormulaValue r (RFDice (RFConstant nr) (RFConstant sides)) =
  let (g', l::[Int]) = mapAccumL (\g _ -> inv $ randomR (1,sides) g) (r^.stdGen) [0..nr-1]
  in (r & stdGen .~ g', Just $ sum l)

resourceFormulaValueF :: (StateFormula -> Int -> Int) -> Run -> ResourceEdgeLabel -> ResourceFormula -> (Run, Maybe Int)
resourceFormulaValueF evalSF r el rf =
  case rawResourceFormulaValue r rf of
    out@(_, Nothing) -> out
    orig@(r', Just val) -> case r^?stateEdgeModifiers.modifyResourceFormula.at el.non [] of
      Nothing -> orig
      -- We don't allow negative values
      Just sf -> (r', Just $ 0 `max` S.foldl' (flip evalSF) val sf)

resourceFormulaValue :: Run -> ResourceEdgeLabel -> ResourceEdge -> (Run, (Maybe Int, Maybe ResourceConstraint))
resourceFormulaValue r el e = (r',(m,e^.constraints))
  where evalSF :: StateFormula -> Int -> Int
        evalSF (SFAdd (SFInterval _)) val = val
        evalSF (SFAdd x) val = val + evalSF x val
        evalSF (SFSub (SFInterval _)) val = val
        evalSF (SFSub x) val = val - evalSF x val
        evalSF (SFConstant c) _ = c
        evalSF f _ = error $ "Unsupported state edge to resource edge formula modifier " <> show f
        (r',m) = resourceFormulaValueF evalSF r el (e^.resourceFormula)

collided :: Resource -> Collision -> Bool
collided res Collision{..} = collisionCollider1 == res || collisionCollider2 == res

data RCEval = RCEBool Bool
            | RCEResources (Set Resource)
            | RCETags (Set Text)
            deriving (Show, Eq)

passesResourceConstraint :: Run -> ResourceConstraint -> Resource -> Bool
passesResourceConstraint r resourceConstraint res =
  case evalRc resourceConstraint of
    Nothing -> False
    (Just (RCEResources [])) -> False
    (Just (RCEResources _)) -> True
    (Just (RCEBool b)) -> b
    (Just (RCETags ts)) -> not $ S.null $ S.filter (res^.tag ==) ts
  where relevantCollisions = S.filter (collided res) (r ^. collisions)
        others = S.map (\c -> if res == c^.collider1 then
                               c^.collider2 else
                               c^.collider1) relevantCollisions
        tyError rc = error $ "Type error in resource constraint " <> show resourceConstraint <> " when evaluating " <> show rc
        evalRc RCCollisionThis = do
          when (S.null relevantCollisions) mzero
          pure $ RCEResources [res]
        evalRc RCCollisionOther = do
          when (S.null relevantCollisions) mzero
          pure $ RCEResources others
        evalRc (RCApply (RCVar "type") rc) = do
          e <- evalRc rc
          case e of
            (RCEResources []) -> mzero
            (RCEResources rs) -> pure $ RCETags $ S.map (^.tag) rs
            _ -> tyError rc
        evalRc (RCTag rc) = pure $ RCETags [rc]
        evalRc rcall@(RCEq rc rc') = do
          e <- evalRc rc
          e' <- evalRc rc'
          case (e,e') of
            (RCETags t, RCETags t') -> pure $ RCEBool $ not $ S.null $ S.intersection t t'
            (RCEResources r, RCEResources r') -> pure $ RCEBool $ not $ S.null $ S.intersection r r'
            _ -> tyError rcall
        evalRc rcall@(RCAnd rc rc') = do
          e <- evalRc rc
          e' <- evalRc rc'
          case (e,e') of
            (RCEBool b, RCEBool b') -> pure $ RCEBool $ b && b'
            _ -> tyError rcall
        evalRc rcall@(RCOr rc rc') = do
          e <- evalRc rc
          e' <- evalRc rc'
          case (e,e') of
            (RCEBool b, RCEBool b') -> pure $ RCEBool $ b || b'
            _ -> tyError rcall

resourceFormulaValueInterval :: Run -> ResourceEdgeLabel -> Interval -> (Run, Maybe Int)
resourceFormulaValueInterval r el i = resourceFormulaValueF evalSF r el (i^.formula)
  where evalSF :: StateFormula -> Int -> Int
        evalSF (SFAdd (SFInterval x)) val = val + evalSF x val
        evalSF (SFAdd _) val = val
        evalSF (SFSub (SFInterval x)) val = val - evalSF x val
        evalSF (SFSub _) val = val
        evalSF (SFConstant c) _ = c
        evalSF f _ = error $ "Unsupported state edge to resource edge formula modifier " <> show f

machinationResources :: Machination -> Set Resource
machinationResources m = S.unions
                         $ map (\n -> case n^.ty of
                                     Pool{..} -> _resources
                                     Delay{..} -> S.fromList $ map _waitingResource _waitingResources 
                                     Queue{..} -> S.fromList $ map _waitingResource _waitingResources
                                     Converter{..} -> S.unions $ M.elems _storage
                                     _ -> []) $ M.elems $ m^.graph.vertices

withCheckingResourceBalance :: Run -> (Run -> Run) -> Run
withCheckingResourceBalance r f =
  if | or /= nr -> error "Original old and new updates must be the same"
     | not $ or' `S.isSubsetOf` nr' ->
       error "In an updated run, the new machine must be a superset of the old"
     | (nr' S.\\ or) /= ((r'^.generatedResources) S.\\ (r'^.killedResources))->
       error $ "Generated resource balance is off: " <> "\n"
             <> show ("new-origial", S.toList $ nr' `S.difference` or) <> "\n"
             <> show ("generated-killed", S.toList $ (r'^.generatedResources) S.\\ (r'^.killedResources)) <> "\n"
             <> show ("New", S.toList $ nr') <> "\n"
             <> show ("Original", S.toList $ or) <> "\n"
             <> show ("Generated", S.toList $ r'^.generatedResources) <> "\n"
             <> show ("Killed", S.toList $ r'^.killedResources) <> "\n"
     | not $ (or S.\\ nr') `S.isSubsetOf` (r'^.killedResources) ->
       error $ "Lost track of killed resources: " <> "\n"
             <> show ("new-origial", S.toList $ nr' `S.difference` or) <> "\n"
             <> show ("generated-killed", S.toList $ (r'^.generatedResources) S.\\ (r'^.killedResources)) <> "\n"
             <> show ("New", S.toList $ nr') <> "\n"
             <> show ("Original", S.toList $ or) <> "\n"
             <> show ("Generated", S.toList $ r'^.generatedResources) <> "\n"
             <> show ("Killed", S.toList $ r'^.killedResources) <> "\n"
     | otherwise -> r'
  where r' = f r
        or = machinationResources (r^.oldUpdate)
        nr = machinationResources (r^.newUpdate)
        or' = machinationResources (r'^.oldUpdate)
        nr' = machinationResources (r'^.newUpdate)

splitMachinationsXml :: FilePath -> Maybe FilePath -> Maybe FilePath -> FilePath -> IO ()
splitMachinationsXml filename convertedFile renderFile destDirectory = do
  Just g <- readMachinationsXml filename
  maybe (pure ()) (\rf -> encodeToFile rf (toGraph g)) renderFile
  maybe (pure ()) (\cf -> B.writeFile cf $ encodePretty g) convertedFile
  let ccs = connectedComponents g
  print $ length ccs
  zipWithM_ (\cc i -> B.writeFile (destDirectory <> printf "%04d" (i :: Int) <> ".json")
                   $ encodePretty cc)
            (sortOn (fst . M.findMin . (^.graph.vertices)) ccs)
            [0..]

renderAllInDirectory :: FilePath -> IO ()
renderAllInDirectory directory = do
  fs <- map (directory</>) . filter ((== ".json") . takeExtension) <$> listDirectory directory
  fms <- catMaybes <$> mapM (\f -> fmap (f,) <$> decodeFileStrict' f) fs
  mapM_ (\(f,m) -> encodeToFile (dropExtension f <> ".dot") (toGraph m)) fms
  mapM_ (\(f,_) -> (flip S.catchany) print $ S.shelly $ S.silently $ S.run_ "dot"
                  ["-Tpdf"
                  , T.pack $ dropExtension f <> ".dot"
                  , "-o"
                  , T.pack $ dropExtension f <> ".pdf"]) fms

resolveAnyLabel' :: Machination -> AnyLabel -> Maybe ResolvedLabel
resolveAnyLabel' m (AnyLabel l) =
  case (m^?graph.vertices.ix (NodeLabel l), m^?graph.resourceEdges.ix (ResourceEdgeLabel l), m^?graph.stateEdges.ix (StateEdgeLabel l)) of
    (Just n, _, _) -> Just $ RNode (NodeLabel l) n
    (_, Just r, _) -> Just $ RResource (ResourceEdgeLabel l) r
    (_, _, Just s) -> Just $ RState (StateEdgeLabel l) s
    _ -> Nothing

resolveAnyLabel :: Machination -> AnyLabel -> ResolvedLabel
resolveAnyLabel m l = fromMaybe (error $ "Unknown node: " <> show l) $ resolveAnyLabel' m l

topologicalSortStateAndRegisters :: Machination -> [Either (NodeLabel, Node) (StateEdgeLabel, StateEdge)]
topologicalSortStateAndRegisters m = map (\v ->
                                            case nodeFromVertex v of
                                              (Left n, Left l, _) -> Left (l,n)
                                              (Right n, Right l, _) -> Right (l,n)
                                              _ -> error "This shouldn't be possible, a bug in our topsort graph")
                                     $ G.topSort g
  where (g, nodeFromVertex, _vertexFromKey) = G.graphFromEdges subgraph
        subgraph :: [(Either Node StateEdge, Either NodeLabel StateEdgeLabel, [Either NodeLabel StateEdgeLabel])]
        subgraph = (map (\(l,n) -> (Left n,Left l,map (Right . fst) $ outStateEdges m l)) $ M.toList
                    $ M.filter isAnyRegister (m^.graph.vertices))
                   <>
                   (map (\(l,e) -> (Right e,Right l,case resolveAnyLabel m $ e^.to of
                                                     RNode nl n -> if isAnyRegister n then [Left nl] else []
                                                     _ -> [])) $ M.toList
                    $ m^.graph.stateEdges)

regenerateFromXmls :: IO ()
regenerateFromXmls = do
  splitMachinationsXml "xmls/101-connections.xml" (Just "ours/101-connections.json") (Just "/tmp/d.dot") "ours/101-connections/"
  splitMachinationsXml "xmls/101-objects.xml" (Just "ours/101-objects.json") (Just "/tmp/d.dot") "ours/101-objects/"
  convertXmlFile "xmls/strange-loop-bug-delay.xml"
  convertXmlFile "xmls/gate-split-66-33.xml"
  convertXmlFile "xmls/tutorials/basic-casual-game-system.xml"
  convertXmlFile "xmls/tutorials/basic-game-idle-system.xml"
  --convertXmlFile "xmls/collisions/test.xml"
  let ds = ["xmls/templates", "xmls/collisions"] :: [[Char]]
  xmls <- foldl (\acc d -> (++) <$> (map (d</>) . filter ((== ".xml") . takeExtension) <$> listDirectory d) <*> acc) (pure []) ds
  mapM_ (\x -> do
            r <- try @SomeException $ convertMachinationsXmlInJSON x
            case r of
              Left ex -> do
                r' <- try @SomeException $ convertXmlFile x
                case r' of
                  Left ex' -> putStrLn $ "Failed to parse " <> show x <> "\n" <> show ex <> "\n" <> show ex'
                  _ -> pure ()
              _ -> pure ()
        ) xmls
  renderAllInDirectory "xmls"
  renderAllInDirectory "xmls/templates"
  renderAllInDirectory "ours"
  renderAllInDirectory "ours/101-connections/"
  renderAllInDirectory "ours/101-objects/"
