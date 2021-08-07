{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyCase, TupleSections, MultiWayIf, RecordWildCards, GADTs #-}
{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fwarn-missing-signatures -Wall -Wno-name-shadowing #-}

module Machinations where
import Machinations.Types
import Machinations.Misc
import Machinations.Xml
import Machinations.Rendering
import Machinations.Formulas
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens hiding (from,to)
import Dot hiding (Graph,Node)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.List (foldl', nubBy, groupBy, sortOn, mapAccumL)
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Control.Monad
import System.Random(mkStdGen,StdGen,random)
import Text.Printf
import System.Directory
import System.FilePath
import qualified Shelly as S
import Data.Bifunctor
import qualified Text.PrettyPrint as P

-- TODO Implement edge inactivation
-- TODO We don't yet handle resources edges with filters like >3
-- TODO Maybe is not good with sampling. The samples will be correlated.
-- TODO Gates are really complex in their deterministic behavior, we aren't there yet.
-- TODO Since we're storing nodes in a Set, the same low-valued resources will
--      tend to move around more. Shuld we implement LIFO? FIFO? etc.
--      Machinations is LIFO (nominally but this isn't always true)
--      Also needed for shuffle origins https://machinations.io/templates/framework/shuffle-source/
-- TODO Test resource balance to make sure that runKilledResources and runGeneratedResources are correct

{-
 The book and the website are inconsitent because they've updated and simplified
 the model since then. There is only one execution mode now, the turn-based one.
 But individual nodes support interval vs instant mode.

 All updates happen simultaneously
 The way to do this is to ensure that all debits happen before all credits
  i.e., a move in the current iteration can't enable something that wasn't possible at the start
 This is complicated by the fact that nodes can interfere with one another
  e.g., only one of two nodes that both debit 1 token from a 1 token pool can run (the website runs the one with lower id)

 To satisfy the above:
  We keep two versions of the machine. Both start off identical.
   The original version is used to check firing conditions
    We apply only debits and not credits to the original version
   The new version will become the final one, so we apply everything to that machine

 NB We add the following to Machinations:
   1. Each resource has an identity for us and can be a real entity (DONE)
   2. We allow for custom user triggers on edges to implement things like collisions (TODO)
    We also do a lot more bookkeeping

 NB We have not yet implemented the sub-actions required for interval mode.

 NB Machinations has strange behavior. Two active nodes can activate a resource connection twice.
     Then it goes over limit for that step.

 NB State connections are always excitatory, never inhibitory.
     In other words, a node that is activated by any state edge, cannot be deactivated by another state edge.

 NB Activation decisions are local. An edge can activate, give something to a gate, then the gate has nothing to do with it. That gate will leak resources!

 NB Nodes can send resources to pools that discard overflow! Regardless of their block status.

 NB Only the conditions of the sender are checked, like pull-any vs pull-all. An activated source can send to a pull-all (because no pull is happening it's just a push!)

 NB Double activations still count as one interval tick!

 NB Execution is greedy and best effort in order of id even if the given order leads to total failure but the opposite order would succeed! A pool with push-all sending to two sinks, one marked 'all' the other '1' will fail depending on the order of the pushes!

 NB State edges cannot be connected to gates

 NB Converters push and pull in the same time step

 NB Activation doens't go past converters backwards, but goes past gates
    But because converters don't hold on to things they go past converters forwards

 NB Gates are strange. When they have no inputs but only state edge outputs they
    randomly enable state edges. When they have a resource as input they act as a
    conduit for shuttling resources aroound. And their ability to control state
    edges goes away, the state edges fire only when the gate is triggered and they always fired.
    These semantics make no sense. Gates should keep their behavior!
     If they're activated they should control state edges as needed.
-}

data Run = Run { runOldUpdate          :: Machination
               , runNewUpdate          :: Machination
               , runActivatedEdges     :: Set ResourceEdgeLabel
               , runFailedEdges        :: Set ResourceEdgeLabel
               , runActivatedNodes     :: Set NodeLabel
               , runFailedNodes        :: Set NodeLabel
               , runTriggeredEdges     :: Set StateEdgeLabel
               , runEdgeFlows          :: Map ResourceEdgeLabel (Set Resource)
               , runGeneratedResources :: Set Resource
               -- TODO Retrofit this everywhere
               , runKilledResources    :: Set Resource
               , runStdGen             :: StdGen
               , runErrors             :: Map AnyLabel Text
               }
  deriving (Show)
makeFields ''Run

summarize :: Run -> String
summarize r = P.render $
  "" P.$$ P.text "Run summary:" P.$$
  P.nest 1 (P.text "Old:" P.$$ (sm $ r^.oldUpdate)
   P.$$
   P.text "New:" P.$$ (sm $ r^.newUpdate)
   P.$$ P.text "Generated" P.<+> P.int (S.size (r^.generatedResources))
   P.$$ P.text "Killed" P.<+> P.int (S.size (r^.killedResources)))
  where sm m = P.nest 2 $ P.vcat $ map sp $ M.toList $ M.filter (\n -> isPool $ n^.ty) $ m^.graph . vertices
        sp (l,Node p@Pool{} _ _) = P.sizedText 6 (show l) P.<+> P.sizedText 6 "Pool" P.<+> P.sizedText 6 (show $ S.size $ _resources p)
        sp _ = ""

-- Full nodes don't count for some operations, like those affecting gates
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

-- What order do things execute in?
-- We activate all of the automatic nodes at the same time and decide what resources they will need and add?
-- It simply can't be that they happen simultaneously!

nodeLookup :: Machination -> NodeLabel -> (NodeLabel, Node)
nodeLookup m l = (l, fromJust $ M.lookup l (m^.graph.vertices))

resourceStatsByTag :: Set Resource -> Map ResourceTag Int
resourceStatsByTag s = M.fromList $ map (\a@(h:_) -> (resourceTag h, length a)) $ groupBy (\x y -> resourceTag x == resourceTag y) $ S.toList s

nodeResources :: Machination -> NodeLabel -> Maybe (Set Resource)
nodeResources m l = m^?graph.vertices.ix l.ty.resources

resurceEdgeLookup :: Machination -> ResourceEdgeLabel -> (ResourceEdgeLabel, ResourceEdge)
resurceEdgeLookup m l = (l, fromJust $ M.lookup l (m^.graph.resourceEdges))

stateEdgeLookup :: Machination -> StateEdgeLabel -> (StateEdgeLabel, StateEdge)
stateEdgeLookup m l = (l, fromJust $ M.lookup l (m^.graph.stateEdges))

outResourceEdges :: Machination -> NodeLabel -> [(ResourceEdgeLabel, ResourceEdge)]
outResourceEdges m l = M.toList $ M.filter (\e -> e^.from == l) (m^.graph.resourceEdges)

inResourceEdges :: Machination -> NodeLabel -> [(ResourceEdgeLabel, ResourceEdge)]
inResourceEdges m l = M.toList $ M.filter (\e -> e^.to == l) (m^.graph.resourceEdges)

automaticNodes :: Machination -> [(NodeLabel, Node)]
automaticNodes = M.toList . M.filter (isJust . (^? (ty.activation._Automatic))) . graphVertices . machinationGraph

startNodes :: Machination -> [(NodeLabel, Node)]
startNodes = M.toList . M.filter (isJust . (^? (ty.activation._OnStart))) . graphVertices . machinationGraph

allReachableNodes :: Ord a => (a -> Set a) -> Set a -> Set a -> Set a
allReachableNodes f candidates seen = loop f (candidates S.\\ seen) (candidates `S.union` seen)
  where loop f as seen | S.null as = seen
                       | otherwise = loop f (S.deleteAt 0 as) (f (S.elemAt 0 as) `S.union` seen)

isActiveResourceEdge :: Run -> ResourceEdgeLabel -> Bool
isActiveResourceEdge _ _ = True

resourceFormulaValue :: Run -> ResourceFormula -> Maybe Int
resourceFormulaValue r RFAll = Nothing
resourceFormulaValue r (RFMultiply x y) = do
  x' <- resourceFormulaValue r x
  y' <- resourceFormulaValue r y
  pure $ x'*y'
resourceFormulaValue r (RFAdd x y) = do
  x' <- resourceFormulaValue r x
  y' <- resourceFormulaValue r y
  pure $ x'+y'
resourceFormulaValue r (RFConstant x) = pure x
resourceFormulaValue r (RFPercentage x) = resourceFormulaValue r x
resourceFormulaValue _ RFCondition{} = Nothing -- TODO These kinds of resource formulas have no value, they're a filter

gateByInterval :: Run -> ResourceEdgeLabel -> ResourceEdge -> Maybe (Run, Set Resource) -> Maybe (Run, Set Resource)
gateByInterval r l e f =
  if fromJust (resourceFormulaValue r (e^?!interval.formula)) >= e^?!interval.counter + 1 then do
      (f', remaining) <- f
      pure (f' & newUpdate . graph . resourceEdges . ix l . interval . counter .~ 0
           ,remaining)
  else
    -- We aren't ready to run, just increment the counter and do nothing
    -- NB This is not a failed edge!
    -- TODO Nor is it an activated edge. What is it?
    Just (r & newUpdate . graph . resourceEdges . ix l . interval . counter +~ 1
         ,[])

type CreditNodeFn' rel nl = Run -> rel -> Set Resource -> nl -> Maybe (Run, Set Resource)
type CreditNodeFn = CreditNodeFn' ResourceEdgeLabel NodeLabel
type CreditNodeFnM = CreditNodeFn' (Maybe ResourceEdgeLabel) (Maybe NodeLabel)
type DebitNodeFn' rel nl = Run -> rel -> Maybe Int -> Maybe ResourceTag -> nl -> Maybe (Run, Set Resource)
type DebitNodeFn = DebitNodeFn' ResourceEdgeLabel NodeLabel
type DebitNodeFnM = DebitNodeFn' (Maybe ResourceEdgeLabel) (Maybe NodeLabel)

distributeByWeight :: CreditNodeFnM -> [(Maybe (NodeLabel, ResourceEdgeLabel), Int)] -> Resource -> Run
                   -> Maybe Run
distributeByWeight creditNode l res r =
  findJust (map (\sample -> fst <$> creditNode r' (snd <$> fst sample) [res] (fst <$> fst sample)) s)
  where (s,g') = weightedShuffle l (r^.stdGen)
        r' = r & stdGen .~ g'

-- TODO This doesn't allow for partial pushes
distributeByWeights :: CreditNodeFnM -> [(Maybe (NodeLabel, ResourceEdgeLabel), Int)] -> Set Resource -> Run
                    -> Maybe Run 
distributeByWeights creditNode l ress r = foldM (flip (distributeByWeight creditNode l)) r (S.toList ress)

-- TODO Replace the counting method with the histogram method with ratios instead of probabilities?
distributeDeterministicGatedResources :: Run -> NodeLabel -> Set Resource -> Maybe Run
distributeDeterministicGatedResources r nodeLabel resources
  | mixedPercentage = mzero
  | allPercentage =
    -- Histogram of edge labels and counts
    -- Inactive edges don't contribute any probability mass
    -- NB This leads to strangess. One edge with 0% probability drops everything. But no edges block.
   case first (filterInactive r) $ fromMaybe mkCounts $ n^?! ty.distribution.counts of
     ([],_) -> mzero -- Can't distribute anything if we have no outgoing edges
     (counts' :: (Map ResourceEdgeLabel Int, Int)) ->
       let targetDistribution_ = M.mapWithKey
                                 (\l _ -> fromIntegral $ fromMaybe 0
                                         $ resourceFormulaValue r (r^?!newUpdate.graph.resourceEdges.ix l.resourceFormula))
                                 (fst counts')
           targetDistribution = (M.map (/100) targetDistribution_, (0 `max` 100 - M.foldl' (+) 0 targetDistribution_)/100)
           onePass' :: Run -> (Map ResourceEdgeLabel Float, Float) -> (Map ResourceEdgeLabel Int, Int) -> Set Resource -> Maybe Run
           onePass' r _ _ [] = Just r
           onePass' r targetDistribution newCounts resources =
             let totalCounts = fromIntegral $ M.foldl' (+) 0 (fst newCounts) + snd newCounts
                 normalize d = case totalCounts of
                   0 -> 0
                   _ -> ((/ totalCounts) . fromIntegral) d
                 currentDistribution = bimap (M.map normalize) normalize newCounts
                 gaps = bimap2 (M.intersectionWith (-)) (-) targetDistribution currentDistribution
                 (mainGaps, leftoverGap) = gaps
             in case mapMaximum mainGaps of
                  Nothing -> mzero -- Technically this is redundant with checking if counts are []
                  Just (minKey,minGap) ->
                    let leftovers = 
                           let newCounts' = second (+1) newCounts
                           in onePass' (r & killedResources <>~ [S.elemAt 0 resources]
                                        & currentNode . ty.distribution.counts ?~ newCounts')
                              targetDistribution newCounts' (S.deleteAt 0 resources)
                        outbound edgeLabel =
                           let (dest :: NodeLabel) = r ^?! oldUpdate . graph . resourceEdges . ix edgeLabel . to
                               newCounts' = first (M.adjust (+1) edgeLabel) newCounts
                           in case creditNode r edgeLabel [S.elemAt 0 resources] dest of
                                Nothing -> onePass' (r & killedResources <>~ [S.elemAt 0 resources]
                                                    & currentNode . ty.distribution.counts ?~ newCounts')
                                          targetDistribution newCounts' (S.deleteAt 0 resources)
                                Just (r',leftover) -> onePass' (r' & killedResources <>~ leftover
                                                               & currentNode . ty.distribution.counts ?~ newCounts')
                                                     targetDistribution newCounts' (S.deleteAt 0 resources)
                    in if | leftoverGap == 0 && minGap == 0 ->
                            case first mapMaximum targetDistribution of
                              (Nothing, _) -> mzero  -- Technically this is redundant with checking if counts are []
                              (Just (maxKey, maxProbability), leftoverProbability) ->
                                if leftoverProbability > maxProbability then
                                  leftovers else
                                  outbound maxKey
                          | leftoverGap > minGap -> leftovers
                          | otherwise -> outbound minKey
       in onePass' r targetDistribution counts' resources
  | otherwise =
   case fromMaybe mkCounts $ n^?! ty.distribution.counts of
     ([],_) -> mzero -- Can't distribute anything if we have no outgoing edges
     (counts' :: (Map ResourceEdgeLabel Int, Int)) ->
       let last = fromMaybe (ResourceEdgeLabel 0) $ n^?! ty.distribution.lastEdge
           capacities = M.mapWithKey (\l _ -> fromMaybe 0 $ resourceFormulaValue r (r^?!newUpdate.graph.resourceEdges.ix l.resourceFormula)) (fst counts')
       in onePass r capacities last counts' resources
  where allOutbound' = outResourceEdges (r^.newUpdate) nodeLabel
        allOutbound = map fst allOutbound'
        n = r ^?! newUpdate . graph . vertices . ix nodeLabel
        currentNode = newUpdate . graph . vertices . ix nodeLabel
        allPercentage = all (isRFPercentage . (^._2.resourceFormula)) allOutbound'
        mixedPercentage = not allPercentage && any (isRFPercentage . (^._2.resourceFormula)) allOutbound'
        mkCounts = (M.fromList $ map (,0) allOutbound, 0)
        destinationNode edgeLabel = oldUpdate . graph . resourceEdges . ix edgeLabel . to
        nodeLookup nodeLabel = oldUpdate . graph . vertices . ix nodeLabel
        filterInactive r m = M.filterWithKey (\k _ -> isActiveResourceEdge r k
                                                     && not (isNodeFull (r ^?! nodeLookup (r ^?! destinationNode k) . ty)))
                             m
        loop :: Run -> Map ResourceEdgeLabel Int -> [ResourceEdgeLabel] -> (Map ResourceEdgeLabel Int, Int) -> Set Resource
             -> (Run, Maybe ResourceEdgeLabel, (Map ResourceEdgeLabel Int, Int), Set Resource)
        loop r _ [] _ resources = (r, Nothing, mkCounts, resources)
        loop r _ (h:_) newCounts [] = (r, Just h, newCounts, [])
        loop r capacities (h:t) (newCounts :: (Map ResourceEdgeLabel Int, Int)) resources
          | (snd (unResourceEdgeLabel h `M.elemAt` fst newCounts) >= snd (unResourceEdgeLabel h `M.elemAt` capacities))
            || not (isActiveResourceEdge r h)
            || isNodeFull (r ^?! oldUpdate . graph . vertices . ix dest . ty)
          = loop r capacities t newCounts resources
          | otherwise =
            let needed = snd (unResourceEdgeLabel h `M.elemAt` capacities) - snd (unResourceEdgeLabel h `M.elemAt` fst newCounts)
                (sent, remaining) = S.splitAt (S.size resources `min` needed) resources
            in case creditNode r h sent dest of
                 Nothing -> loop r capacities t (first (M.updateAt (\_ -> pure . (+ S.size sent)) (unResourceEdgeLabel h)) newCounts) remaining
                 Just (r',_) -> loop r' capacities t (first (M.updateAt (\_ -> pure . (+ S.size sent)) (unResourceEdgeLabel h)) newCounts) remaining
          where (dest :: NodeLabel) = r ^?! oldUpdate . graph . resourceEdges . ix (fst $ unResourceEdgeLabel h `M.elemAt` fst newCounts) . to
        onePass :: Run -> Map ResourceEdgeLabel Int -> ResourceEdgeLabel -> (Map ResourceEdgeLabel Int, Int) -> Set Resource -> Maybe Run
        onePass r capacities last newCounts resources =
             case loop r capacities (map ResourceEdgeLabel $ rotate (unResourceEdgeLabel last) [0..M.size (fst newCounts)-1]) newCounts resources of
               (r', last', newCounts', []) -> updateRun r' newCounts' last'
               (r', Nothing, newCounts', leftover) -> if newCounts' /= newCounts then
                                                       onePass r' capacities (ResourceEdgeLabel 0) newCounts' leftover else
                                                       onePass r' capacities (ResourceEdgeLabel 0) newCounts' leftover -- TODO updateRun r' newCounts' (Just 0)
        updateRun r' counts' lastEdge' =
             pure $ r' & newUpdate . graph . vertices . ix nodeLabel . ty . distribution . counts ?~ counts'
                       & newUpdate . graph . vertices . ix nodeLabel . ty . distribution . lastEdge .~ lastEdge'

-- TODO This doesn't allow for partial pushes
distributeGatedResources :: Run -> NodeLabel -> Set Resource -> Maybe Run
distributeGatedResources r nodeLabel resources =
  (case n^?! ty.distribution of
     Random{} -> distributeRandomGatedResources
     Deterministic{} -> distributeDeterministicGatedResources) r nodeLabel resources
  where n = r ^?! newUpdate . graph . vertices . ix nodeLabel

distributeRandomGatedResources :: Run -> NodeLabel -> Set Resource -> Maybe Run
distributeRandomGatedResources r nodeLabel resources = do
  -- You can't mix weights and percentages because two weights 25 and 25 result
  -- in an event split. But two percentages 25% and 25% result in 50% of the
  -- resources being distributed evenly but the rest being dropped.
  -- TODO We should probably switch out of Maybe so that we can propagate errors
  when mixedPercentage mzero
  let tempWeights = mapMaybe (\e -> (Just $ (snd e ^. to, fst e),) <$> resourceFormulaValue r (e^._2.resourceFormula)) es
  let weights = if allPercentage && sum (map snd tempWeights) < 100 then
                  (Nothing, 100-sum (map snd tempWeights)) : tempWeights
                  else
                  tempWeights
  r' <- distributeByWeights creditNode' weights resources r
  pure $ r' & activatedNodes <>~ [nodeLabel]
  where es = filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.newUpdate) nodeLabel
        allPercentage = all (isRFPercentage . (^._2.resourceFormula)) es
        mixedPercentage = not allPercentage && any (isRFPercentage . (^._2.resourceFormula)) es
        -- Discard anything that's not being sent to a node
        creditNode' r _ res   Nothing = Just (r & killedResources <>~ res, [])
        creditNode' r Nothing res _ = Just (r & killedResources <>~ res, [])
        creditNode' r (Just e) res (Just l) = creditNode r e res l

generateResources :: Run -> ResourceTag -> Int -> (Run, Set Resource)
generateResources r tag nr = 
      let (newGen,createdResources) = second S.fromList
            $ mapAccumL (\g _ -> inv $ first (Resource tag) $ mkUuid' g) (r^.stdGen) ([1..nr] :: [Int])
      in (r & generatedResources <>~ createdResources
            & stdGen .~ newGen
         , createdResources)

debitNode :: Run -> ResourceEdgeLabel -> Maybe Int -> Maybe ResourceTag -> NodeLabel -> Maybe (Run, Set Resource)
debitNode r l amount resourceTag from =
  case n^.ty of
    Drain{} -> Nothing
    Source{} -> do
      resourceTagToGenerate <-
        case n ^?! ty . resourceTypes of
          [] -> Nothing
          [tag] -> case resourceTag of
                    Nothing -> pure tag
                    -- TODO Handle generating multiple resources
                    Just neededTag -> if tag == neededTag then
                                       pure tag else
                                       Nothing
      -- TODO 10k is an odd default in machinations.io
      --      I set it to 100 here
      let (r', createdResources) = generateResources r resourceTagToGenerate $ fromMaybe 100 amount
      pure (r' & activatedNodes <>~ [from]
           , createdResources)
    Pool{} -> do
      let filteredResources =
            case resourceTag of
              Nothing -> n ^?! ty . resources
              Just tags -> S.filter (\r -> r^.tag == tags) $ n ^?! ty . resources
      let (out, remaining) = case amount of
            Nothing -> (filteredResources, [])
            Just amount -> S.splitAt (amount `min` S.size filteredResources) filteredResources
      pure (r & oldUpdate . graph . vertices . ix from . ty . resources .~ remaining
              & newUpdate . graph . vertices . ix from . ty . resources %~ (S.\\ out)
              & activatedNodes <>~ [from]
           ,out)
    Converter{} ->
      (,[]) <$> fireConverterIfPossible r from
  where n = r ^?! oldUpdate . graph . vertices . ix from

creditNode :: Run -> ResourceEdgeLabel -> Set Resource -> NodeLabel -> Maybe (Run, Set Resource)
creditNode r sourceEdge incoming to =
  -- TODO Check limits on the credited node and fail or overflow
  case n'^.ty of
    Source{} -> Nothing
    Drain{} -> pure (r & activatedNodes <>~ [to], [])
    p@Pool{} -> do
      let overCapacity = not $
            case p^?!limit of
              Nothing -> True
              Just maximum ->
                maximum >=
                S.size ((r ^. newUpdate . graph . vertices . ix to . ty . resources)
                         <> incoming)
      if overCapacity then
        case p^?!overflow of
          OverflowBlock -> mzero
          OverflowDrain -> pure (r & activatedNodes <>~ [to]
                                  & killedResources <>~ incoming -- TODO Update for partial pushes
                               , [])
        else pure (r & newUpdate . graph . vertices . ix to . ty . resources <>~ incoming
                    & activatedNodes <>~ [to]
                  ,[])
    Gate{} ->
      -- TOOD This doesn't allow for partial pushes
      (,[]) <$> distributeGatedResources r to incoming
    Converter{} ->
      -- NB One of the few cases where we update both the old and the new!
      -- This allows converters to operate without a delay
      (,[]) <$>
       fireConverterIfPossible (r & newUpdate . graph . vertices . ix to . ty . storage . at sourceEdge . non [] <>~ incoming
                                  & oldUpdate . graph . vertices . ix to . ty . storage . at sourceEdge . non [] <>~ incoming)
                                        to
  where n' = r ^?! newUpdate . graph . vertices . ix to

fireConverterIfPossible :: Run -> NodeLabel -> Maybe Run
fireConverterIfPossible r l = 
  if all (\((elabel::ResourceEdgeLabel),(edge :: ResourceEdge)) ->
            -- TODO Should this default to 0 or 1?
             maybe 0 S.size (available ^? ix elabel) >= fromMaybe 1 (resourceFormulaValue r (edge^.resourceFormula))
          ) (allInboundOld :: [(ResourceEdgeLabel,ResourceEdge)])
     then
    case allOutboundOld of
      [] -> Just $ clearStorage r
      [(olabel, oedge)] -> do
        case n^.resourceTypes of
          [rtag] ->
            -- TODO Is the default here 0 or 1?
            let (r', res) = generateResources r rtag (fromMaybe 0 $ resourceFormulaValue r (oedge^.resourceFormula))
            in Just $ maybe (clearStorage r') fst $ creditNode (clearStorage r') olabel res (oedge^.to)
          _ -> pure r
      _ -> Nothing -- Only one active outbound edge
    else
    pure r
  where -- NB Inactive inbound edges count. Inactive outbound edges do not!
        allInboundOld = inResourceEdges (r^.oldUpdate) l
        allOutboundOld = filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.newUpdate) l
        n = r ^?! oldUpdate . graph . vertices . ix l . ty
        available = n^.storage
	clearStorage r = r & oldUpdate . graph . vertices . ix l . ty . storage .~ []
                           & newUpdate . graph . vertices . ix l . ty . storage .~ []

runResourceEdge :: DebitNodeFn -> CreditNodeFn
                -> Bool -> Node -> Run -> (ResourceEdgeLabel, ResourceEdge)
                -> Maybe (Run, Set Resource)
-- TODO Resources edges propagate activation to non-latched nodes
runResourceEdge debitFn creditFn maxNeeded n r (l,e) =
  -- TODO this only applies to the latched nodes
    if | isLatched $ oldSource^.ty ->
         gateByInterval r l e $ do
           let amount = resourceFormulaValue r (e^.resourceFormula)
           -- TODO Check limits on the credited node so we don't ask for too much
           -- TODO Check limits on resource edge
           (r',resources) <- debitFn r l amount (e^.resourceFilter) (e^.from)
           when (maxNeeded && Just (S.size resources) /= amount) mzero
           (r'',remainingResources) <- creditFn r' l resources (e^.to)
           pure (r'' & activatedEdges <>~ [l]
                     & edgeFlows . at l . non S.empty <>~ resources
                , remainingResources)
       | isGate $ oldSource^.ty -> mzero -- TODO?
       | isConverter $ oldSource^.ty -> mzero -- TODO?
  where oldSource = r ^?! oldUpdate . graph . vertices . ix (e^.from)
        oldTarget = r ^?! oldUpdate . graph . vertices . ix (e^.from)

runNode :: Run -> (NodeLabel, Node) -> Run
runNode r (l, n) =
  case n^.ty of
    p@Pool{} ->
      case _pushPullAction p of
        Pushing PushAny -> pushPullAny allOutboundOld
        Pushing PushAll -> pushPullAll allOutboundOld
        Pulling PullAny -> pushPullAny allInboundOld
        Pulling PullAll -> pushPullAll allInboundOld
    Source{} -> pushPullAny allOutboundOld
    s@Drain{} -> case _pullAction s of
                  PullAny -> pushPullAny allInboundOld
                  PullAll -> pushPullAll allInboundOld
    s@Gate{} -> -- TODO State edges
      case (allInboundOld, allOutboundOld) of
        ([],_) -> r
        (_,[]) -> r
        (_,_) -> case (case _pullAction s of
                    PullAny -> pushPullAny' allInboundOld dontcredit
                    PullAll -> pushPullAll' allInboundOld dontcredit) of
              (r', resources) -> -- We fail if we can't distribue the resources anywhere
                fromMaybe r $ distributeGatedResources r' l resources
    s@Converter{} -> case _pullAction s of
                  PullAny -> pushPullAny allInboundOld
                  PullAll -> pushPullAll allInboundOld
  where -- NB Inactive resources edges don't count
        allInboundOld = filter (isActiveResourceEdge r . fst) $ inResourceEdges (r^.oldUpdate) l
        allOutboundOld = filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.newUpdate) l
        dontcredit = \r _ s _ -> Just (r,s)
        runResourceEdge' c d m n (r,s) e = do
          (r', s') <- runResourceEdge c d m n r e
          pure (r', s <> s')
        pushPullAny' edges creditFn =
          case foldM (runResourceEdge' debitNode creditFn False n)
               (r, []) $ filter (isActiveResourceEdge r . fst) edges of
            Nothing -> (r & failedEdges <>~ S.fromList (map fst edges)
                         & failedNodes <>~ [l]
                      , [])
            Just (r, s) -> (r & activatedNodes <>~ [l], s)
        pushPullAny edges = fst $ pushPullAny' edges creditNode
        pushPullAll' rawEdges creditFn =
          let edges = filter (isActiveResourceEdge r .fst) rawEdges
          in -- NB Disabled resource edges don't count toward PushAll/PullAll requirements!
            case foldM (runResourceEdge' debitNode creditFn True n)
                 (r, []) edges of
              Nothing -> (r & failedEdges <>~ S.fromList (map fst edges)
                           & failedNodes <>~ [l]
                        , [])
              Just (r, s) -> (r & activatedNodes <>~ [l], s)
        pushPullAll edges = fst $ pushPullAll' edges creditNode

run :: Machination -> [NodeLabel] -> Run
run m clicked =
  let r = loop (Run m m S.empty S.empty S.empty S.empty S.empty M.empty S.empty S.empty (mkStdGen $ m^.seed) M.empty)
               (automaticNodes m
                <> (if m^.time == 0 then startNodes m else [])
                <> map (nodeLookup m) clicked)
          & newUpdate . time +~ 1
  in r & newUpdate . seed .~ fst (random $ r^.stdGen)
  where loop :: Run -> [(NodeLabel, Node)] -> Run
        loop m active = foldl' runNode m active

exSourcePoolDrain :: NodeActivation -> NodeActivation -> PushPullAction -> NodeActivation -> Int -> Int -> Machination
exSourcePoolDrain sa pa pt da rsp rpd =
  Machination (Graph
                [(NodeLabel 0, Node (Source sa ["life"]) "Source" "black")
                ,(NodeLabel 1, Node (Pool pa pt [Resource "life" "1"] OverflowBlock Nothing) "Pool" "black")
                ,(NodeLabel 2, Node (Drain da PullAny) "Drain" "black")
                ]
                [(ResourceEdgeLabel 100, ResourceEdge (NodeLabel 0) (NodeLabel 1) (RFConstant rsp) (Interval (RFConstant 1) 0) IntervalTransfer Nothing False (Limits Nothing Nothing))
                ,(ResourceEdgeLabel 101, ResourceEdge (NodeLabel 1) (NodeLabel 2) (RFConstant rpd) (Interval (RFConstant 1) 0) IntervalTransfer Nothing False (Limits Nothing Nothing))]
                [])
            [("life", "red")] 0 0

startHere :: Machination
startHere = exSourcePoolDrain Automatic Passive (Pushing PushAny) Passive 3 0

splitMachinationsXml :: FilePath -> Maybe FilePath -> Maybe FilePath -> FilePath -> IO ()
splitMachinationsXml filename convertedFile renderFile destDirectory = do
  Just g <- readMachinationsXml filename
  -- print g
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
  mapM_ (\(f,_) -> S.shelly $ S.silently $ S.run_ "dot"
                  ["-Tpdf"
                  , T.pack $ dropExtension f <> ".dot"
                  , "-o"
                  , T.pack $ dropExtension f <> ".pdf"]) fms
