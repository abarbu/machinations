{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyCase, TupleSections, MultiWayIf, RecordWildCards, GADTs #-}
{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fwarn-missing-signatures -Wall -Wno-name-shadowing #-}

module Machinations where
import Machinations.Types
import Machinations.Utils
import Machinations.Misc
import Machinations.Formulas
import Control.Lens hiding (from,to)
import Data.Maybe
import Data.List (foldl', sortOn, mapAccumL, (\\), delete, nub)
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Control.Monad
import System.Random(random)
import Data.Bifunctor

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

 NB Resource edge draws are not cached! In other words Source -> Pool where both
    the source and the pool are activated with an edge that has a D5 on it, will
    draw different values for every activation of the resource edge.

 NB State edges resolve at the end of execution
-}

gateByInterval :: Run -> ResourceEdgeLabel -> ResourceEdge -> (Run -> Maybe (Run, Set Resource)) -> Maybe (Run, Set Resource)
gateByInterval r l e f =
  case resourceFormulaValue r (e^?!interval.formula) of
    (r', Just val) ->
      if val <= e^?!interval.counter + 1 then do
        (f', remaining) <- f r'
        pure (f' & newUpdate . graph . resourceEdges . ix l . interval . counter .~ 0
             ,remaining)
      else
        -- We aren't ready to run, just increment the counter and do nothing
        -- NB This is not a failed edge!
        -- TODO Nor is it an activated edge. What is it?
        Just $ (r' & newUpdate . graph . resourceEdges . ix l . interval . counter +~ 1
               ,[])
    _ -> Nothing -- Intervals need to be numbers

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
       let (r', targetDistribution_) = M.mapAccumWithKey
                                       (\r l _ -> second (fromIntegral . fromMaybe 0)
                                                 $ resourceFormulaValue r (r^?!newUpdate.graph.resourceEdges.ix l.resourceFormula))
                                       r
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
       in onePass' r' targetDistribution counts' resources
  | otherwise =
   case fromMaybe mkCounts $ n^?! ty.distribution.counts of
     ([],_) -> mzero -- Can't distribute anything if we have no outgoing edges
     (counts' :: (Map ResourceEdgeLabel Int, Int)) ->
       let last = fromMaybe (ResourceEdgeLabel 0) $ n^?! ty.distribution.lastEdge
           (r', capacities) = M.mapAccumWithKey (\r l _ -> second (fromMaybe 0)
                                                          $ resourceFormulaValue r (r^?!newUpdate.graph.resourceEdges.ix l.resourceFormula))
                              r
                              (fst counts')
       in onePass r' capacities last counts' resources
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
                 Nothing ->     loop r  capacities t (first (M.updateAt (\_ -> pure . (+ S.size sent)) (unResourceEdgeLabel h)) newCounts) remaining
                 Just (r',l) -> loop (r' & killedResources <>~ l) capacities t (first (M.updateAt (\_ -> pure . (+ S.size sent)) (unResourceEdgeLabel h)) newCounts) remaining
          where (dest :: NodeLabel) = r ^?! oldUpdate . graph . resourceEdges . ix (fst $ unResourceEdgeLabel h `M.elemAt` fst newCounts) . to
        onePass :: Run -> Map ResourceEdgeLabel Int -> ResourceEdgeLabel -> (Map ResourceEdgeLabel Int, Int) -> Set Resource -> Maybe Run
        onePass r capacities last newCounts resources =
             case loop r capacities (map ResourceEdgeLabel $ rotate (unResourceEdgeLabel last) [0..M.size (fst newCounts)-1]) newCounts resources of
               (r', last', newCounts', []) -> updateRun r' newCounts' last'
               (r', Nothing, newCounts', leftover) -> if newCounts' /= newCounts then
                                                       onePass r' capacities (ResourceEdgeLabel 0) newCounts' leftover else
                                                       onePass r' capacities (ResourceEdgeLabel 0) newCounts' leftover
                                                       -- TODO updateRun r' newCounts' (Just 0)
                                                       -- Why isn't this the right move here?
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
  let (r', tempWeights) = second catMaybes
                        $ mapAccumL (\r e ->
                                       second
                                       (fmap (Just $ (snd e ^. to, fst e),))
                                       (resourceFormulaValue r (e^._2.resourceFormula)))
                          r es
  let weights = if allPercentage && sum (map snd tempWeights) < 100 then
                  (Nothing, 100-sum (map snd tempWeights)) : tempWeights
                  else
                  tempWeights
  distributeByWeights creditNode' weights resources r'
  where es = filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.newUpdate) nodeLabel
        allPercentage = all (isRFPercentage . (^._2.resourceFormula)) es
        mixedPercentage = not allPercentage && any (isRFPercentage . (^._2.resourceFormula)) es
        -- Discard anything that's not being sent to a node
        creditNode' r _ res   Nothing = Just (r & killedResources <>~ res, [])
        creditNode' r Nothing res _ = Just (r & killedResources <>~ res, [])
        creditNode' r (Just e) res (Just l) = creditNode r e res l

traderInputsOutputs :: Run -> NodeLabel -> Node -> Maybe (((ResourceEdgeLabel, ResourceEdge, Filter),
                                                       (ResourceEdgeLabel, ResourceEdge, Filter)),
                                                      ((ResourceEdgeLabel, ResourceEdge, Filter),
                                                       (ResourceEdgeLabel, ResourceEdge, Filter)))
traderInputsOutputs r l _ = 
      case (allInboundOld, allOutboundOld) of
        -- You can only run with exactly 2 inputs and 2 outputs
        ([(il1,ie1),(il2,ie2)],[(ol1,oe1),(ol2,oe2)]) ->
          case (ie1^.resourceFilter,ie2^.resourceFilter,oe1^.resourceFilter,oe2^.resourceFilter) of
            -- They must all have filters
            (Just if1, Just if2, Just of1, Just of2) ->
              -- And edges must be matched in pairs
              if | if1 == if2 || of1 == of2 -> Nothing
                 | if1 == of1 && if2 == of2 -> Just (((il1,ie1,if1),(ol1,oe1,of1)),((il2,ie2,if2),(ol2,oe2,of2)))
                 | if1 == of2 && if2 == of1 -> Just (((il1,ie1,if1),(ol2,oe2,of2)),((il2,ie2,if2),(ol1,oe1,of1)))
                 | otherwise -> Nothing
            _ -> Nothing
        _ -> Nothing
  where allInboundOld = filter (isActiveResourceEdge r . fst) $ inResourceEdges (r^.oldUpdate) l
        allOutboundOld = filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.oldUpdate) l

generateResources :: Run -> ResourceTag -> Int -> (Run, Set Resource)
generateResources r tag nr = 
      let (newGen,createdResources) = second S.fromList
            $ mapAccumL (\g _ -> inv $ first (Resource tag) $ mkUuid' g) (r^.stdGen) ([1..nr] :: [Int])
      in (r & generatedResources <>~ createdResources
            & stdGen .~ newGen
         , createdResources)

fireConverterIfPossible :: Run -> NodeLabel -> Maybe Run
fireConverterIfPossible r l =
  let (r', inResourceEdgeValues) =
        mapAccumL (\r (elabel,edge) -> second (((elabel,edge),) . fromMaybe 0) (resourceFormulaValue r (edge^.resourceFormula)))
                  r
        allInboundOld
  in if all (\((elabel,_),evalue) ->
               -- TODO Should this default to 0 or 1?
                maybe 0 S.size (available ^? ix elabel) >= evalue
             ) inResourceEdgeValues
        then
          case allOutboundOld of
            [] -> Just $ clearStorage r'
            [(olabel, oedge)] -> do
              case n^.resourceTypes of
                [rtag] ->
                  -- TODO Is the default here 0 or 1?
                  let (r'', val) = second (fromMaybe 0) $ resourceFormulaValue r' (oedge^.resourceFormula)
                      (r''', res) = generateResources r'' rtag val
                  in Just $ maybe (clearStorage r''') fst $ creditNode (clearStorage r''') olabel res (oedge^.to)
                _ -> pure r'
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
                           & killedResources <>~ (S.unions $ M.elems $ r ^. oldUpdate . graph . vertices . ix l . ty . storage)

debitNode :: Run -> ResourceEdgeLabel -> Maybe Int -> Maybe ResourceTag -> NodeLabel -> Maybe (Run, Set Resource)
debitNode r destEdge amount resourceTag from | not $ isActiveNode r from = Nothing
                                             | otherwise = updateNodeStats $
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
      pure $ generateResources r resourceTagToGenerate $ fromMaybe 100 amount
    Pool{} -> do
      let filteredResources =
            case resourceTag of
              Nothing -> n ^?! ty . resources
              Just tags -> S.filter (\r -> r^.tag == tags) $ n ^?! ty . resources
      let (out, remaining) = case amount of
            Nothing -> (filteredResources, [])
            Just amount -> S.splitAt (amount `min` S.size filteredResources) filteredResources
      pure (r & oldUpdate . graph . vertices . ix from . ty . resources %~ (S.\\ out) -- .~ remaining
              & newUpdate . graph . vertices . ix from . ty . resources %~ (S.\\ out)
           ,out)
    Converter{} ->
      (,[]) <$> fireConverterIfPossible r from
  where n = r ^?! oldUpdate . graph . vertices . ix from
        updateNodeStats Nothing = Nothing
        updateNodeStats (Just (r, debited)) =
          Just (r & edgeflow . at destEdge . non [] <>~ debited
                  & nodeOutflow . at from . non [] <>~ debited
               , debited)

creditNode :: Run -> ResourceEdgeLabel -> Set Resource -> NodeLabel -> Maybe (Run, Set Resource)
creditNode r sourceEdge incoming toNode | not $ isActiveNode r toNode = Nothing
                                        | otherwise = updateNodeStats $
  -- TODO Check limits on the credited node and fail or overflow
  case n'^.ty of
    Source{} -> Nothing
    Drain{} -> pure (r & killedResources <>~ incoming
                   , [])
    p@Pool{} -> do
      let overCapacity = maybe False (< S.size ((r ^. newUpdate . graph . vertices . ix toNode . ty . resources) <> incoming))
                                     (p^?!limit)
      if overCapacity then
        case p^?!overflow of
          OverflowBlock -> mzero
          OverflowDrain -> pure (r & killedResources <>~ incoming -- TODO Update for partial pushes
                               , [])
        else pure (r & newUpdate . graph . vertices . ix toNode . ty . resources <>~ incoming
                  ,[])
    Gate{} ->
      -- TOOD This doesn't allow for partial pushes
      (,[]) <$> distributeGatedResources r toNode incoming
    Converter{} ->
      -- NB One of the few cases where we update both the old and the new!
      -- This allows converters to operate without a delay
      (,[]) <$>
       fireConverterIfPossible (r & newUpdate . graph . vertices . ix toNode . ty . storage . at sourceEdge . non [] <>~ incoming
                                  & oldUpdate . graph . vertices . ix toNode . ty . storage . at sourceEdge . non [] <>~ incoming)
                               toNode
    Trader{} -> do
      ((i1,o1),(i2,o2)) <- traderInputsOutputs r toNode (r ^?! oldUpdate . graph . vertices . ix toNode)
      (_,o) <- (if
                  | sourceEdge == i1^._1 -> Just (i1,o1)
                  | sourceEdge == i2^._1 -> Just (i2,o2)
                  | otherwise -> Nothing)
      -- We use up all the resources no matter what happens
      -- pure $ fromMaybe (r, []) creditNode
      creditNode r (o^._1) incoming (o^._2.to)
    Delay{} ->
      case filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.oldUpdate) toNode of
        [outEdge] ->
          let now = r^.oldUpdate.time
              (r', val) = second (fromMaybe 0) $ resourceFormulaValue r (outEdge^._2.resourceFormula)
          in Just $ (,[]) $ r' & newUpdate . graph . vertices . ix toNode . ty . waitingResources <>~ map (Waiting $ now + val) (S.toList incoming)
    Queue{} ->
      case filter (isActiveResourceEdge r . fst) $ outResourceEdges (r^.oldUpdate) toNode of
        [outEdge] ->
          let (r', val) = second (fromMaybe 0) $ resourceFormulaValue r (outEdge^._2.resourceFormula)
          in Just $ (,[]) $ r' & newUpdate . graph . vertices . ix toNode . ty . waitingResources <>~ map (Waiting $ now + val) (S.toList incoming)
  where n' = r ^?! newUpdate . graph . vertices . ix toNode
        now = r^.oldUpdate.time
        updateNodeStats Nothing = Nothing
        updateNodeStats (Just (r, leftovers)) =
          Just (r & edgeflow . at sourceEdge . non [] <>~ incoming S.\\ leftovers
                  & nodeInflow . at toNode . non [] <>~ incoming S.\\ leftovers
               , leftovers)

runResourceEdge :: DebitNodeFn -> CreditNodeFn
                -> Bool -> Node -> Run -> (ResourceEdgeLabel, ResourceEdge)
                -> Maybe (Run, Set Resource)
-- TODO Resources edges propagate activation to non-latched nodes
runResourceEdge debitFn creditFn maxNeeded _ r (l,e) =
  -- TODO this only applies to the latched nodes
    if | isLatched $ oldSource^.ty ->
         gateByInterval r l e $ \r -> do
           let (r', amount) = resourceFormulaValue r (e^.resourceFormula)
           -- TODO Check limits on the credited node so we don't ask for too much
           -- TODO Check limits on resource edge
           (r'',resources) <- debitFn r' l amount (e^.resourceFilter) (e^.from)
           when (maxNeeded && Just (S.size resources) /= amount) mzero
           (r''',remainingResources) <- creditFn r'' l resources (e^.to)
           pure (r''' & activatedEdges <>~ [l]
                      & edgeflow . at l . non S.empty <>~ resources
                , remainingResources)
       | isGate $ oldSource^.ty -> mzero -- TODO?
       | isConverter $ oldSource^.ty -> mzero -- TODO?
       | isTrader $ oldSource^.ty -> mzero -- TODO?
  where oldSource = r ^?! oldUpdate . graph . vertices . ix (e^.from)
        oldTarget = r ^?! oldUpdate . graph . vertices . ix (e^.from)

pushDelayedResources :: Run -> ResourceEdgeLabel -> NodeLabel -> Node -> NodeType -> NodeLabel -> Run
pushDelayedResources r edgel l _ d dest =
  case alive of
    [] -> r
    _ -> case creditNode r edgel (S.fromList $ map (^.resource) alive) dest of
          Nothing -> r -- We hold on to the resources
          Just (r, leftovers) ->
            r & oldUpdate . graph . vertices . ix l . ty . waitingResources %~
                   (\prev -> let dead = filter (\a -> not ((a^.resource) `S.member` leftovers)) alive
                            in prev \\ dead)
              & newUpdate . graph . vertices . ix l . ty . waitingResources %~
                   (\prev -> let dead = filter (\a -> not ((a^.resource) `S.member` leftovers)) alive
                            in prev \\ dead)
  where alive = filter (\x -> x^.startTime <= r^.oldUpdate.time) $ d^.waitingResources

pushQueuedResources :: Run -> (ResourceEdgeLabel, ResourceEdge) -> NodeLabel -> Node -> NodeType -> NodeLabel -> Run
pushQueuedResources r (edgel,edge) l _ d dest =
  case alive of
    [] -> r
    (h:_) -> 
      if h^.startTime <= now && maybe True (now >=) (d^?!nextTimeAvailable) then
        case creditNode r' edgel [h^.resource] dest of
              Nothing -> r' -- We hold on to the resources
              Just (r'', []) -> r'' & oldUpdate . graph . vertices . ix l . ty . waitingResources %~ delete h
                                   & newUpdate . graph . vertices . ix l . ty . waitingResources %~ delete h
                                   & oldUpdate . graph . vertices . ix l . ty . nextTimeAvailable ?~ now+val
                                   & newUpdate . graph . vertices . ix l . ty . nextTimeAvailable ?~ now+val
              Just (_, [_]) -> r -- there can only be one, we abort
              _ -> error "pushQueuedResources shouldn't be able to have more leftovers"
      else
        r
  where alive = filter (\x -> x^.startTime <= r^.oldUpdate.time) $ d^.waitingResources
        now = r^.oldUpdate.time
        (r', val) = second (fromMaybe 0) $ resourceFormulaValue r (edge^.resourceFormula)

runNode :: Run -> (NodeLabel, Node) -> Run
runNode r (l, n) | not $ isActiveNode r l = r
                 | otherwise =
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
    Trader{} -> case traderInputsOutputs r l n of
                 Just ((i1,_),(i2,_)) -> pushPullAll [(i1^._1,i1^._2),(i2^._1,i2^._2)]
                 Nothing -> r
    d@Delay{} -> case allOutboundOld of
      [dest] -> pushDelayedResources (pushPullAny allInboundOld) (dest^._1) l n d (dest^._2.to)
      _ -> r -- We fail if there are multiple outputs
    q@Queue{} -> case allOutboundOld of
      [dest] -> pushQueuedResources (pushPullAny allInboundOld) dest l n q (dest^._2.to)
      _ -> r -- We fail if there are multiple outputs
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

resolveAnyLabel :: Machination -> AnyLabel -> ResolvedLabel
resolveAnyLabel m (AnyLabel l) =
  case (m^?graph.vertices.ix (NodeLabel l), m^?graph.resourceEdges.ix (ResourceEdgeLabel l), m^?graph.stateEdges.ix (StateEdgeLabel l)) of
    (Just n, _, _) -> RNode (NodeLabel l) n
    (_, Just r, _) -> RResource (ResourceEdgeLabel l) r
    (_, _, Just s) -> RState (StateEdgeLabel l) s
    _ -> error "Unknown node"

postUpdateStateEdge :: Run -> (StateEdgeLabel, StateEdge) -> Run
postUpdateStateEdge r (sel, se) =
  case (resolveAnyLabel (r^.oldUpdate) $ se^.from, resolveAnyLabel (r^.oldUpdate) $ se^.to) of
    (RNode nlfrom nfrom, RNode nlto nto) ->
      case se^.stateFormula of
        -- Triggers and reverse triggers are not symmetric
        -- A trigger fires when a node is activated or when all of its active inputs are satisfied
        --  If it has any active inputs (zero input nodes can't meet this second condition)
        -- A reverse trigger fires when a node is activated but fails
        --  A node that doesn't receive all of its inputs does not trigger this condition
        Just SFTrigger        -> if (nlfrom `S.member` (r^.activatedNodes))
                                   ||
                                   (all (\(e,_) -> maybe False ((>0) . S.size) $ r^?edgeflow.ix e)
                                    (filter (isActiveResourceEdge r . fst) $ inResourceEdges (r^.oldUpdate) nlfrom))
                                then trigger r nlto else r
        Just SFReverseTrigger -> if nlfrom `S.member` (r^.failedNodes) then trigger r nlto else r
        _ -> r
    -- TODO
    _ -> r
  where trigger :: Run -> NodeLabel -> Run
        trigger r nlabel = r & newUpdate . stateEdgeModifiers . non mkStateEdgeModifiers . triggerNode <>~ [nlabel]

preUpdateStateEdge :: Machination -> StateEdgeModifiers -> (StateEdgeLabel, StateEdge) -> StateEdgeModifiers
preUpdateStateEdge m sem (sel, se) =
  case (resolveAnyLabel m $ se^.from, resolveAnyLabel m $ se^.to) of
    (RNode nlfrom nfrom, RNode nlto nto) ->
      case se^.stateFormula of
        Just (SFRange (SFConstant low) (SFConstant high)) ->
          case nfrom of
            Node Pool{..} _ _ ->
              if S.size _resources >= low && S.size _resources <= high then
                activateNode sem nlto
              else
                inactivateNode sem nlto
        Just (SFCondition c (SFConstant val)) ->
          case nfrom of
            Node Pool{..} _ _ ->
              if (case c of
                    CEqual -> (==)
                    CNotEqual -> (/=)
                    CGt -> (>)
                    CLt -> (<)
                    CGtEq -> (>=)
                    CLtEq -> (<=))
                 (S.size _resources)
                 val
              then activateNode sem nlto
              else inactivateNode sem nlto
        -- TODO
        _ -> sem
    (RNode nlfrom nfrom, RResource rlto rto) ->
      case se^.stateFormula of
        Just (SFRange (SFConstant low) (SFConstant high)) ->
          case nfrom of
            Node Pool{..} _ _ ->
              if S.size _resources >= low && S.size _resources <= high then
                activateEdge sem rlto
              else
                inactivateEdge sem rlto
        Just (SFCondition c (SFConstant val)) ->
          case nfrom of
            Node Pool{..} _ _ ->
              if (case c of
                    CEqual -> (==)
                    CNotEqual -> (/=)
                    CGt -> (>)
                    CLt -> (<)
                    CGtEq -> (>=)
                    CLtEq -> (<=))
                 (S.size _resources)
                 val
              then activateEdge sem rlto
              else inactivateEdge sem rlto
        -- TODO
        _ -> sem
    -- TODO
    _ -> sem
  where activateNode :: StateEdgeModifiers -> NodeLabel -> StateEdgeModifiers
        activateNode sem nlabel = sem & enableNode <>~ [nlabel]
        inactivateNode :: StateEdgeModifiers -> NodeLabel -> StateEdgeModifiers
        inactivateNode sem nlabel = sem & disableNode <>~ [nlabel]
        activateEdge :: StateEdgeModifiers -> ResourceEdgeLabel -> StateEdgeModifiers
        activateEdge sem nlabel = sem & enableResourceEdge <>~ [nlabel]
        inactivateEdge :: StateEdgeModifiers -> ResourceEdgeLabel -> StateEdgeModifiers
        inactivateEdge sem nlabel = sem & disableResourceEdge <>~ [nlabel]

run :: Machination -> Set NodeLabel -> Run
run m clicked =
  withCheckingResourceBalance (mkRun m') $ \r0 ->
      let r = loop r0
                   (nub
                    $ automaticNodes m'
                    <> (if m^.time == 0 then startNodes m' else [])
                    <> map (nodeLookup m') (S.toList clicked)
                    <> maybe [] (map (nodeLookup m) . S.toList) (m ^? stateEdgeModifiers._Just.triggerNode)
                   )
          r' = foldl' postUpdateStateEdge r $ M.toList $ r^.newUpdate. graph. stateEdges
      in r' & newUpdate . seed .~ fst (random $ r^.stdGen)
  where loop :: Run -> [(NodeLabel, Node)] -> Run
        loop m active = foldl' runNode m active
        m' = m & time +~ 1
               & stateEdgeModifiers ?~ accumOverStateEdges  mkStateEdgeModifiers (preUpdateStateEdge m) (M.toList $ m^.graph. stateEdges)
        accumOverStateEdges init f edges = foldl' f init edges
          -- foldl' f init edges

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
            [("life", "red")] 0 0 Nothing

startHere :: Machination
startHere = exSourcePoolDrain Automatic Passive (Pushing PushAny) Passive 3 0

