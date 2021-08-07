{-# LANGUAGE OverloadedLists, OverloadedStrings, TypeApplications, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Machinations.Rendering where
import Machinations.Types
import Machinations.Xml
import Data.Aeson.Encode.Pretty
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens hiding (from,to)
import qualified Dot as D
import Dot hiding (Graph,Node)
import Data.String
import qualified Data.ByteString.Lazy as B
import Data.List (foldl', groupBy, sortOn)
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad
import Text.Printf

show' = fromString . show

showf f | f - fromIntegral (round f) < 10e-6 = fromString $ show $ round f
        | otherwise = show' f

instance Semigroup Id where
  Id a <> Id b = Id $ a <> b

nodeToStatement :: NodeLabel -> Node -> Statement
nodeToStatement l n =
  StatementNode $ NodeStatement (show' l)
    [ Attribute "color" (Id $ n^.color)
    , Attribute "shape" (Id $ case n^.ty of
                            Source{} -> "triangle"
                            Drain{} -> "invtriangle"
                            Pool{} -> "circle"
                            Converter{} -> "cds"
                            Trader{} -> "star"
                            EndCondition{} -> "Msquare"
                            Delay{} -> "promoter"
                            Queue{} -> "rarrow"
                            Gate{} -> "diamond"
                            RegisterFn{} -> "box3d"
                            RegisterInteractive{} -> "box3d"
                        )
    , Attribute "peripheries" (case n^?ty.activation of
                                  Just Interactive -> "2"
                                  _ -> "1")
    , HtmlAttribute "label" ((case n^.ty of
                                 Source{} -> ""
                                 Drain{} -> ""
                                 t@Pool{} -> show' $ S.size $ t^.resources
                                 Converter{} -> ""
                                 EndCondition{} -> ""
                                 Trader{} -> ""
                                 Delay{} -> ""
                                 Queue{} -> ""
                                 Gate{} -> ""
                                 r@RegisterFn{} -> maybe "f(x)<BR />" (Id . formulaToLabel) (r^?registerFormula)
                                 RegisterInteractive{} -> "Interactive<BR />"
                             )
                             <>
                             (case n^.label of
                               "" -> ""
                               l -> Id $ "<BR />" <> l)
                              <>
                             (case n^?ty.activation of
                                  Just Automatic -> "<SUP>*</SUP>"
                                  Just OnStart -> "<SUP>S</SUP>"
                                  _ -> "")
                             <>
                            (case n^?ty.pushPullAction of
                                  Just (Pushing p) -> labelPush p
                                  Just (Pulling p) -> labelPull p
                                  _ -> "")
                            )
    , Attribute "labelfontcolor" (case n^.ty of
                                     Source{} -> "black"
                                     Drain{} -> "black"
                                     Converter{} -> "black"
                                     EndCondition{} -> "black"
                                     Gate{} -> "black"
                                     Trader{} -> "black"
                                     Delay{} -> "black"
                                     Queue{} -> "black"
                                     RegisterFn{} -> "black"
                                     RegisterInteractive{} -> "black"
                                     t@Pool{} -> case map (resourceTag . head)
                                                     $ groupBy (\a b -> resourceTag a == resourceTag b)
                                                     $ S.toList
                                                     $ t^.resources of
                                                  [] -> "black"
                                                  [res] -> Id res)
      -- TODO Other properties
    ]
  where labelPush PushAny = "<SUB>p</SUB>"
        labelPush PushAll = "<SUB>p&amp;</SUB>"
        labelPull PullAll = "<SUB>&amp;</SUB>"
        labelPull _ = ""

formulaToLabel :: Formula -> Text
formulaToLabel (FVar v) = v
formulaToLabel (FConstant i) = show' i
formulaToLabel (FApply x y) = formulaToLabel x <> "(" <> formulaToLabel y <> ")"
formulaToLabel (FPair x y) = formulaToLabel x <> "," <> formulaToLabel y
formulaToLabel (FNeg x) = "-" <> formulaToLabel x
formulaToLabel (FAdd x y) = formulaToLabel x <> "+" <> formulaToLabel y
formulaToLabel (FSub x y) = formulaToLabel x <> "-" <> formulaToLabel y
formulaToLabel (FMul x y) = formulaToLabel x <> "*" <> formulaToLabel y
formulaToLabel (FDiv x y) = formulaToLabel x <> "/" <> formulaToLabel y

resourceFormulaToLabel :: ResourceFormula -> Text
resourceFormulaToLabel RFAll = "all"
resourceFormulaToLabel (RFMultiply x y) = resourceFormulaToLabel x <> "*" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFDivide x y) = resourceFormulaToLabel x <> "/" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFAdd x y) = resourceFormulaToLabel x <> "+" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFSubtract x y) = resourceFormulaToLabel x <> "-" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFNegation x) = "-" <> resourceFormulaToLabel x
resourceFormulaToLabel (RFPercentage x) = resourceFormulaToLabel x <> "%"
resourceFormulaToLabel (RFDice (RFConstant 1) y) = "D" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFDice x y) = resourceFormulaToLabel x <> "D" <> resourceFormulaToLabel y
resourceFormulaToLabel (RFConstant f) = show' f

resourceEdgeToStatement :: ResourceEdgeLabel -> ResourceEdge -> [Statement]
resourceEdgeToStatement l e =
  [StatementNode $ NodeStatement (show' l)
    [ Attribute "shape" "septagon"
    , HtmlAttribute "label" ((case e^?resourceFormula of
                                Just rf -> Id $ resourceFormulaToLabel rf)
                              <>
                             (case e^?interval.formula of
                                Just (RFConstant 1) -> ""
                                Just i -> " | " <> show' i
                                _ -> ""))
    , Attribute "peripheries" (case e^.resourceFilter of
                                  Just _ -> "2"
                                  _ -> "1")
    , color
    ]
  ,StatementEdge $ EdgeStatement (ListTwo (EdgeNode $ show' $ e^.from) (EdgeNode $ show' l) [])
    [ color ]
  ,StatementEdge $ EdgeStatement (ListTwo (EdgeNode $ show' l) (EdgeNode $ show' $ e^.to) [])
    [ color ]]
  where color = Attribute "color" (case e^.resourceFilter of
                                  Just r -> Id r
                                  _ -> "black")

conditionToLabel CEqual = "=="
conditionToLabel CNotEqual = "!="
conditionToLabel CGt = "&gt;" -- >
conditionToLabel CLt = "&lt;" -- <
conditionToLabel CGtEq = "&gt;="
conditionToLabel CLtEq = "&lt;="

stateFormulaToLabel (SFAdd x) = "+" <> stateFormulaToLabel x
stateFormulaToLabel (SFSub x) = "-" <> stateFormulaToLabel x
stateFormulaToLabel (SFMul x) = "*" <> stateFormulaToLabel x
stateFormulaToLabel (SFDiv x) = "/" <> stateFormulaToLabel x
stateFormulaToLabel (SFCondition c x) = conditionToLabel c <> stateFormulaToLabel x
stateFormulaToLabel (SFOverwrite x) = "=" <> stateFormulaToLabel x
stateFormulaToLabel (SFPercentage x) = stateFormulaToLabel x <> "%"
stateFormulaToLabel (SFRange x y) =  stateFormulaToLabel x <> ".." <> stateFormulaToLabel y
stateFormulaToLabel (SFInterval x) = stateFormulaToLabel x <> "i"
stateFormulaToLabel (SFConstant i) = show' i
stateFormulaToLabel SFTrigger = "*"
stateFormulaToLabel SFReverseTrigger = "!"
stateFormulaToLabel (SFVariable i) = i

stateEdgeToStatement :: StateEdgeLabel -> StateEdge -> [Statement]
stateEdgeToStatement l e =
  [StatementNode $ NodeStatement (show' l)
    [ Attribute "shape" "septagon"
    , HtmlAttribute "label" (case e^?stateFormula of
                                Just rf -> Id $ maybe "" stateFormulaToLabel rf)
    , Attribute "peripheries" (case e^.resourceFilter of
                                  Just _ -> "2"
                                  _ -> "1")
    , color
    , style
    ]
  ,StatementEdge $ EdgeStatement (ListTwo (EdgeNode $ show' $ e^.from) (EdgeNode $ show' l) [])
    [ color, style ]
  ,StatementEdge $ EdgeStatement (ListTwo (EdgeNode $ show' l) (EdgeNode $ show' $ e^.to) [])
    [ color, style ]]
  where color = Attribute "color" (case e^.resourceFilter of
                                  Just r -> Id r
                                  _ -> "black")
        style = Attribute "style" (Id $ T.intercalate "," $ ["dashed"]
                          <> if e^.active then ["bold"] else [])

toGraph :: Machination -> DotGraph
toGraph m = DotGraph NonStrict Directed Nothing
                               ([ StatementAttribute $ AttributeStatement D.Graph
                                  [ Attribute "label" ("step=" <> show' (m^.time))
                                  , Attribute "labelloc" "bottom"
                                  , Attribute "labeljust" "left"
                                  , Attribute "fontsize" "10"
                                  ]
                                ] ++
                                map (uncurry nodeToStatement) (M.toList $ m ^. graph.vertices) ++
                                concatMap (uncurry resourceEdgeToStatement) (M.toList $ m ^. graph.resourceEdges) ++
                                concatMap (uncurry stateEdgeToStatement) (M.toList $ m ^. graph.stateEdges))

-- -- NB All of these will share the same seed!
connectedComponents :: Machination -> [Machination]
connectedComponents m = loop (S.map toAnyLabel $ M.keysSet $ m^.graph.vertices)
  where loop [] = []
        loop ns =
          let ls = connectedComponent vs rs ss (S.elemAt 0 ns)
          in m { machinationGraph =
                   Graph { graphVertices = M.mapKeys toNodeLabel $ M.filterWithKey (\k _ -> k `S.member` ls) vs 
                         , graphResourceEdges = M.mapKeys toResourceEdgeLabel $ M.filterWithKey (\k _ -> k `S.member` ls) rs
                         , graphStateEdges = M.mapKeys toStateEdgeLabel $ M.filterWithKey (\k _ -> k `S.member` ls) ss
                         }
               } : loop (ns S.\\ ls)
        vs = M.mapKeys toAnyLabel $ m^.graph.vertices
        rs = M.mapKeys toAnyLabel $ m^.graph.resourceEdges
        ss = M.mapKeys toAnyLabel $ m^.graph.stateEdges

connectedComponent :: Map AnyLabel Node
                   -> Map AnyLabel ResourceEdge
                   -> Map AnyLabel StateEdge
                   -> AnyLabel
                   -> Set AnyLabel
connectedComponent _ rs ss current = loop current []
  where connections :: AnyLabel -> Set AnyLabel
        -- All edges and nodes directly connected to this label
        connections l =
          let rs' = M.filter (\r -> r^.from == toNodeLabel l) rs `M.union` M.filter (\r -> r^.to == toNodeLabel l) rs
              ss' = M.filter (\r -> r^.from == l) ss `M.union` M.filter (\r -> r^.to == l) ss
          in S.fromList $ map (toAnyLabel . (^.to)) (M.elems rs') <> map (toAnyLabel . (^.from)) (M.elems rs')
                       <> map (^.to) (M.elems ss') <> map (^.from) (M.elems ss')
                       <> M.keys rs' <> M.keys ss'
        loop :: AnyLabel -> Set AnyLabel -> Set AnyLabel
        loop current seen =
          case connections current S.\\ seen of
            [] -> S.insert current seen
            cs -> foldl' (flip loop) (S.insert current $ S.union cs seen) cs
