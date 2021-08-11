{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, EmptyCase, TupleSections, MultiWayIf, RecordWildCards, GADTs #-}
{-# LANGUAGE RankNTypes, OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fwarn-missing-signatures -Wall -Wno-name-shadowing #-}

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
import qualified Data.Map.Strict as M
import Control.Monad
import System.Random(randomR)
import System.Directory
import System.FilePath
import qualified Shelly as S
import qualified Data.ByteString.Lazy as B
import Data.Text(Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Dot hiding (Graph,Node)
import Text.Printf
import Machinations.Xml

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

inResourceEdges :: Machination -> NodeLabel -> [(ResourceEdgeLabel, ResourceEdge)]
inResourceEdges m l = M.toList $ M.filter (\e -> e^.to == l) (m^.graph.resourceEdges)

automaticNodes :: Machination -> [(NodeLabel, Node)]
automaticNodes = M.toList . M.filter (isJust . (^? (ty.activation._Automatic))) . graphVertices . machinationGraph

startNodes :: Machination -> [(NodeLabel, Node)]
startNodes = M.toList . M.filter (isJust . (^? (ty.activation._OnStart))) . graphVertices . machinationGraph

-- | If anything activates the edge, it's active, even if something else inactivates it
isActiveResourceEdge :: Run -> ResourceEdgeLabel -> Bool
isActiveResourceEdge r l | isJust $ r^?newUpdate.stateEdgeModifiers._Just.enableResourceEdge.ix l = True
                         | isJust $ r^?newUpdate.stateEdgeModifiers._Just.disableResourceEdge.ix l = False
                         | otherwise = True

-- | If anything activates the node, it's active, even if something else inactivates it
isActiveNode :: Run -> NodeLabel -> Bool
isActiveNode r l | isJust $ r^?newUpdate.stateEdgeModifiers._Just.enableNode.ix l = True
                 | isJust $ r^?newUpdate.stateEdgeModifiers._Just.disableNode.ix l = False
                 | otherwise = True

-- TODO This is wonky, should separate out [all, int, percentage, and condition] but machinations doesn't!
resourceFormulaValue :: Run -> ResourceFormula -> (Run, Maybe Int)
resourceFormulaValue r RFAll = (r, Nothing)
resourceFormulaValue r (RFMultiply x y) = do
  let (r', x')  = resourceFormulaValue r x
      (r'', y') = resourceFormulaValue r' y
    in (r'', liftM2 (*) x' y')
resourceFormulaValue r (RFAdd x y) = do
  let (r', x')  = resourceFormulaValue r x
      (r'', y') = resourceFormulaValue r' y
    in (r'', liftM2 (+) x' y')
resourceFormulaValue r (RFConstant x) = (r, pure x)
resourceFormulaValue r (RFPercentage x) = resourceFormulaValue r x
  -- TODO These kinds of resource formulas have no value, they're a filter
resourceFormulaValue _ RFCondition{} = error "I don't undertand what conditions on resource edges mean"
resourceFormulaValue r (RFDice (RFConstant nr) (RFConstant sides)) =
  let (g', l::[Int]) = mapAccumL (\g _ -> inv $ randomR (1,sides) g) (r^.stdGen) [0..nr-1]
  in (r & stdGen .~ g', Just $ sum l)

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
