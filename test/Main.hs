{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# OPTIONS -Wall #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Machinations
import Machinations.Types
import Machinations.Misc
import Machinations.Formulas
import Machinations.Utils
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import System.IO.Unsafe
import System.FilePath
import Data.Bifunctor
import Data.List
import Control.Lens hiding (from,to)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [object101Tests,connections101Tests,rfTests,sfTests,spdTests,miscTests]

readMachination' :: FilePath -> Machination
readMachination' = fromJust . unsafePerformIO . decodeFileStrict'
noderes n g = resourceStatsByTag <$> nodeResources g (NodeLabel n)
run' g = runNewUpdate $ run g []
run2' = run' . run'
runN' n x = iterate run' x !! n
runN n m activate | n>0 = loop (n-1) (run m activate)
                  | otherwise = error "At least one run is necessary"
  where loop 0 r = r
        loop n r = loop (n-1) (run (r^.newUpdate) activate)

testOneNodeResourcesRaw :: Int -> Int -> String -> Maybe (Map ResourceTag Int) -> TestTree
testOneNodeResourcesRaw node steps file right =
  testCase (show node <> " x" <> show steps <> " " <> file)
  $ noderes node (runN' steps $ readMachination' file) @?= right

testOneNodeResources :: Int -> Int -> String -> Map ResourceTag Int -> TestTree
testOneNodeResources node steps file right = testOneNodeResourcesRaw node steps file (Just right)

testNodeResourcesAndRegistersRaw :: Int -> String -> [(Int, Maybe (Map ResourceTag Int))] -> [(Int, Maybe Double)] -> TestTree
testNodeResourcesAndRegistersRaw steps file noderights registerrights =
  let r = runN steps (readMachination' file) []
      m = r^.newUpdate
  in testGroup (show (map fst noderights) <> " x" <> show steps <> " " <> file)
     $ map (\(node,right) ->
               testCase (show node <> " x" <> show steps)
               $ noderes node m @?= right)
       noderights
       <>
       map (\(register,right) ->
              testCase (show register <> " x" <> show steps)
              $ M.lookup (NodeLabel register) (r^.registerValues) @?= right)
       registerrights

testNodeResources :: Int -> String -> [(Int, Map ResourceTag Int)] -> TestTree
testNodeResources steps file regs =
  testNodeResourcesAndRegistersRaw steps file (map (second Just) regs) []

testNodeResourcesAndRegisters :: Int -> String -> [(Int, Map ResourceTag Int)] -> [(Int, Double)] -> TestTree
testNodeResourcesAndRegisters steps file nodes regs =
  testNodeResourcesAndRegistersRaw steps file (map (second Just) nodes) (map (second Just) regs)

testEnded :: Int -> String -> Int -> Bool -> Set Int -> TestTree
testEnded steps file node isEnded triggered =
  let r = runN steps (readMachination' file) (S.map NodeLabel triggered)
  in testCase ("end " <> show node <> " x" <> show steps <> " " <> file)
     $ r^.ended @?= if isEnded then Just (NodeLabel node) else Nothing

object101Tests = testGroup "101-objects"
  [
    testGroup "sources"
    [
      testRaw 526 1 "0088.json" Nothing
    , test 518 1 "0084.json" []
    , test 520 1 "0085.json" []
    , test 521 1 "0086.json" [("Black",1)]
    , test 521 2 "0086.json" [("Black",2)]
    , test 522 1 "0087.json" [("Black",1)]
    , test 522 2 "0087.json" [("Black",1)]
    ]
  , testGroup "drains"
    [
      testRaw 471 1 "0074.json" Nothing
    , test 482 1 "0075.json" [("Black", 10)]
    , test 476 1 "0076.json" [("Black", 10)]
    , test 477 1 "0077.json" [("Black", 9)]
    , test 477 2 "0077.json" [("Black", 8)]
    , test 480 1 "0078.json" [("Black", 9)]
    , test 480 2 "0078.json" [("Black", 9)]
    , test 495 1 "0079.json" [("Black", 1)]
    , test 495 2 "0079.json" [("Black", 2)]
    , test 489 1 "0080.json" [("Black", 1)]
    , test 489 2 "0080.json" [("Black", 2)]
    , test 490 1 "0081.json" [("Black", 1)]
    , test 490 2 "0081.json" [("Black", 2)]
    , test 490 5 "0081.json" [("Black", 5)]
    , test 490 6 "0081.json" [("Black", 1)]
    , test 507 1 "0083.json" [("Black", 1)]
    , test 507 2 "0083.json" [("Black", 1)]
    , test 493 1 "0082.json" [("Black", 1)]
    , test 493 2 "0082.json" [("Black", 2)]
    ]
  , testGroup "pools"
    [
      testGroup "pull-any"
      [
        test 353 1 "0049.json" []
      , test 349 1 "0046.json" []
      , test 350 1 "0047.json" [("Black", 1)]
      , test 350 2 "0047.json" [("Black", 2)]
      , test 351 1 "0048.json" [("Black", 1)]
      , test 351 2 "0048.json" [("Black", 1)]
      , test 396 1 "0058.json" [("Red", 2)]
      ]
    , testGroup "push-any"
      [
        test 366 1 "0053.json" []
      , test 362 1 "0050.json" []
      , test 363 1 "0051.json" []
      , test 364 1 "0052.json" []
      , test 380 1 "0057.json" [("Black", 10)]
      , test 380 2 "0057.json" [("Black", 10)]
      , test 375 1 "0054.json" [("Black", 10)]
      , test 375 2 "0054.json" [("Black", 10)]
      , test 376 1 "0055.json" [("Black", 10)]
      , test 376 2 "0055.json" [("Black", 9)]
      , test 377 1 "0056.json" [("Black", 10)]
      , test 377 2 "0056.json" [("Black", 10)]
      ]
    , testGroup "pull-all"
      [
        test 421 1 "0064.json" [("Black", 1)]
      , test 422 1 "0064.json" []
      , test' 1 "0064.json" [(421, [("Black", 1)]),(422, [])]
      , test' 2 "0064.json" [(421, [("Black", 2)]),(422, [])]
      , test' 6 "0064.json" [(421, [("Black", 6)]),(422, [])]
      , test' 1 "0065.json" [(413, [("Black", 1)]),(414, [])]
      , test' 2 "0065.json" [(413, [("Black", 2)]),(414, [])]
      , test' 6 "0065.json" [(413, [("Black", 6)]),(414, [])]
      , test' 1 "0066.json" [(416, [("Black", 1)]),(417, [])]
      , test' 2 "0066.json" [(416, [("Black", 2)]),(417, [])]
      , test' 6 "0066.json" [(416, [("Black", 1)]),(417, [("Black", 5)])]
      , test' 1 "0072.json" [(454, [("Black", 1)]),(455, [])]
      , test' 2 "0072.json" [(454, [("Black", 1)]),(455, [("Black", 1)])]
      , test' 6 "0072.json" [(454, [("Black", 1)]),(455, [("Black", 5)])]
      , test' 1 "0067.json" [(419, [("Black", 1)]),(420, [])]
      , test' 2 "0067.json" [(419, [("Black", 2)]),(420, [])]
      , test' 6 "0067.json" [(419, [("Black", 6)]),(420, [])]
      ]
    , testGroup "push-all"
      [
        test' 1 "0068.json" [(442, [("Black", 1)]),(443, [])]
      , test' 5 "0068.json" [(442, [("Black", 5)]),(443, [])]
      , test' 6 "0068.json" [(442, [("Black", 6)]),(443, [])]
      , test' 1 "0069.json" [(434, [("Black", 1)]),(435, [])]
      , test' 5 "0069.json" [(434, [("Black", 5)]),(435, [])]
      , test' 6 "0069.json" [(434, [("Black", 6)]),(435, [])]
      , test' 1 "0070.json" [(437, [("Black", 1)]),(438, [])]
      , test' 5 "0070.json" [(437, [("Black", 5)]),(438, [])]
      , test' 6 "0070.json" [(437, [("Black", 1)]),(438, [("Black", 5)])]
      , test' 1 "0071.json" [(440, [("Black", 1)]),(441, [])]
      , test' 5 "0071.json" [(440, [("Black", 5)]),(441, [])]
      , test' 6 "0071.json" [(440, [("Black", 6)]),(441, [])]
      ]
    , test' 1 "0073.json" [(465, [("Black", 1)]),(467, [])]
    , test' 2 "0073.json" [(465, [("Black", 2)]),(467, [])]
    , test' 5 "0073.json" [(465, [("Black", 5)]),(467, [])]
    , testGroup "capacity limits"
      [
        test' 1 "0062.json" [(403, [("Black", 9)]),(404, [("Black", 1)])]
      , test' 4 "0062.json" [(403, [("Black", 6)]),(404, [("Black", 4)])]
      , test' 5 "0062.json" [(403, [("Black", 6)]),(404, [("Black", 4)])]
      , test' 1 "0063.json" [(407, [("Black", 9)]),(408, [("Black", 1)])]
      , test' 4 "0063.json" [(407, [("Black", 6)]),(408, [("Black", 4)])]
      , test' 5 "0063.json" [(407, [("Black", 5)]),(408, [("Black", 4)])]
      ]
    , testGroup "Mutli-edge & multi-activation"
      [
        test' 1 "0089.json" [(532, [("Black", 8)])]
      , test' 2 "0089.json" [(532, [("Black", 13)])]
      , test' 3 "0089.json" [(532, [("Black", 18)])]
      ]
    ]
  , testGroup "gates"
    [
      -- NB The actual values here assume a seed of zero, reseeding at every
      -- iteration and that non-realized paths don't update the seed (which is
      -- bad)
      testGroup "pull-any"
      [
        test' 1 "0029.json" [(228, [])]
      , test' 1 "0030.json" [(225, [])]
      , test' 1 "0031.json" [(233, [("Black", 1)])]
      , test' 3 "0031.json" [(233, [("Black", 3)])]
      , test' 1 "0032.json" [(238, [("Black", 1)])]
      , test' 5 "0032.json" [(238, [("Black", 1)])]
      , test' 100 "0033.json" [(245, [("Black", 50)]), (247, [("Black", 50)])]
      , test' 100 "0034.json" [(251, [("Black", 491)]), (252, [("Black", 509)])]
      , test' 1 "0040.json" [(299, [("Black", 1)])]
      , test' 5 "0040.json" [(299, [("Black", 5)])]
      , test' 5 "0041.json" [(305, [("Black", 5)])]
      , test' 1 "0035.json" [(259, [("Black", 1)]), (261, [])]
      , test' 2 "0035.json" [(259, [("Black", 1)]), (261, [("Black", 1)])]
      , test' 3 "0035.json" [(259, [("Black", 2)]), (261, [("Black", 1)])]
      , test' 4 "0035.json" [(259, [("Black", 2)]), (261, [("Black", 2)])]
      , test' 100 "0036.json" [(266, [("Black", 52)]), (268, [("Black", 48)])]
      , test' 100 "0037.json" [(273, []), (275, [])]
      , test' 1 "0038.json" [(281, []), (283, [])]
      , test' 2 "0038.json" [(281, [("Black", 1)]), (283, [])]
      , test' 3 "0038.json" [(281, [("Black", 1)]), (283, [("Black", 1)])]
      , test' 4 "0038.json" [(281, [("Black", 1)]), (283, [("Black", 1)])]
      , test' 5 "0038.json" [(281, [("Black", 1)]), (283, [("Black", 1)])]
      , test' 100 "0039.json" [(290, [("Black", 50)]), (292, [("Black", 50)])]
      , test' 5 "0042.json" [(311, [("Black", 5)]), (313, []), (314, [])]
      , test' 6 "0042.json" [(311, [("Black", 1)]), (313, [("Black", 2)]), (314, [("Black", 3)])]
      , test' 5 "0043.json" [(320, [("Black", 5)]), (322, []), (323, [])]
      , test' 6 "0043.json" [(320, [("Black", 1)]), (322, [("Black", 2)]), (323, [("Black", 3)])]
      , test' 1 "0091.json" [(550, [("Black", 3)]), (545, [("Black", 2)])]
      , test' 2 "0091.json" [(550, [("Black", 6)]), (545, [("Black", 4)])]
      , test' 3 "0091.json" [(550, [("Black", 10)]) , (545, [("Black", 5)])]
      , test' 5 "gate-split-66-33.json" [(107, [("Black", 2)]) ,(106, [("Black", 1)])]
      ]
    ]
  , testGroup "converters"
    [
      test 123 1 "0015.json" []
    , test 126 1 "0016.json" []
    , test 134 1 "0017.json" [("Black",1)]
    , test 138 1 "0018.json" [("Black",15)]
    , test 217 1 "0026.json" [("Black",1)]
    , test 209 1 "0027.json" [("Black",1)]
    , test' 1 "0019.json" [(142, [("Black", 1)]), (146, [])]
    , test' 2 "0019.json" [(142, [("Black", 1)]), (146, [])]
    , test' 4 "0019.json" [(142, [("Black", 1)]), (146, [])]
    , test' 5 "0019.json" [(142, [("Black", 1)]), (146, [])]
    , test' 6 "0019.json" [(142, [("Black", 1)]), (146, [("Black", 1)])]
    , test' 1 "0020.json" [(149, [("Black", 1)]), (153, [])]
    , test' 2 "0020.json" [(149, [("Black", 2)]), (153, [])]
    , test' 5 "0020.json" [(149, [("Black", 5)]), (153, [])]
    , test' 6 "0020.json" [(149, [("Black", 1)]), (153, [("Black", 1)])]
    , test' 1 "0021.json" [(157, [("Black", 1)]), (164, [("Black", 1)]), (161, [])]
    , test' 2 "0021.json" [(157, [("Black", 1)]), (164, [("Black", 1)]), (161, [])]
    , test' 3 "0021.json" [(157, [("Black", 1)]), (164, [("Black", 1)]), (161, [])]
    , test' 5 "0021.json" [(157, [("Black", 1)]), (164, [("Black", 1)]), (161, [])]
    , test' 6 "0021.json" [(157, [("Black", 1)]), (164, [("Black", 1)]), (161, [("Black", 1)])]
    , test' 1 "0022.json" [(168, [("Black", 1)]), (175, [("Black", 1)]), (172, [])]
    , test' 2 "0022.json" [(168, [("Black", 2)]), (175, [("Black", 2)]), (172, [])]
    , test' 3 "0022.json" [(168, [("Black", 3)]), (175, [("Black", 3)]), (172, [])]
    , test' 5 "0022.json" [(168, [("Black", 5)]), (175, [("Black", 5)]), (172, [])]
    , test' 6 "0022.json" [(168, [("Black", 1)]), (175, [("Black", 4)]), (172, [("Black", 1)])]
    , test' 6 "0024.json" [(182, [("Black", 6)]), (186, []), (188, [])] -- TODO Test node failure
    , test' 1 "0025.json" [(192, [("Black", 1)]), (198, []), (200, [])]
    , test' 5 "0025.json" [(192, [("Black", 5)]), (198, []), (200, [])]
    , test' 6 "0025.json" [(192, [("Black", 1)]), (198, [("Black", 5)]), (200, [("Black", 3)])]
    , test' 1 "0090.json" [(538, [("Black", 15)]), (542, [])]
    , test' 2 "0090.json" [(538, [("Black", 26)]), (542, [("Black", 1)])]
    , test' 3 "0090.json" [(538, [("Black", 37)]), (542, [("Black", 2)])]
    ]
  , testGroup "traders"
    [
      test' 1 "0013.json" [(101, [("Black",1),("Blue",3),("Green",2),("Orange",3),("Red",3)])
                          ,(102, []),(103, []),(104, []),(105, []),(106, [])]
    , test' 2 "0013.json" [(101, [("Black",3),("Blue",3),("Green",3),("Orange",2),("Red",3)])
                          ,(102, [("Red",1)]),(103, [("Orange",3)]),(104, [("Green",2)]),(105, [("Blue",2)]),(106, [("Black",1)])]
    , test' 1 "0009.json" [(52, [("Blue",1)]),(55, []),(58, [("Black",10)]),(57, [])]
    , test' 5 "0009.json" [(52, [("Blue",5)]),(55, []),(58, [("Black",10)]),(57, [])]
    , test' 6 "0009.json" [(52, [("Blue",6)]),(55, []),(58, [("Black",10)]),(57, [])]
    , test' 1 "0010.json" [(63, [("Blue",1)]),(66, []),(69, [("Black",10)]),(68, [])]
    , test' 5 "0010.json" [(63, [("Blue",5)]),(66, []),(69, [("Black",10)]),(68, [])]
    , test' 6 "0010.json" [(63, [("Blue",6)]),(66, []),(69, [("Black",10)]),(68, [])]
    , test' 1 "0011.json" [(74, [("Blue",1)]),(77, []),(80, [("Black",10)]),(79, [])]
    , test' 5 "0011.json" [(74, [("Blue",5)]),(77, []),(80, [("Black",10)]),(79, [])]
    , test' 6 "0011.json" [(74, [("Blue",1)]),(77, [("Blue",5)]),(80, [("Black",9)]),(79, [("Black",1)])]
    , test' 1 "0012.json" [(85, [("Blue",1)]),(88, []),(91, [("Black",10)]),(90, [])]
    , test' 5 "0012.json" [(85, [("Blue",5)]),(88, []),(91, [("Black",10)]),(90, [])]
    , test' 6 "0012.json" [(85, [("Blue",6)]),(88, []),(91, [("Black",10)]),(90, [])]
    ]
  , testGroup "delay"
    [
      test' 1 "0000.json" [(6, [("Black",10)]),(9, [])]
    , test' 2 "0000.json" [(6, [("Black",10)]),(9, [])]
    , test' 3 "0000.json" [(6, [("Black",10)]),(9, [])]
    , test' 4 "0000.json" [(6, [("Black",10)]),(9, [])]
    , test' 1 "0001.json" [(11, [("Black",10)]),(14, [])]
    , test' 2 "0001.json" [(11, [("Black",10)]),(14, [])]
    , test' 3 "0001.json" [(11, [("Black",10)]),(14, [])]
    , test' 4 "0001.json" [(11, [("Black",10)]),(14, [])]
    , test' 1 "0002.json" [(16, [("Black",9)]),(19, [])]
    , test' 2 "0002.json" [(16, [("Black",8)]),(19, [])]
    , test' 3 "0002.json" [(16, [("Black",7)]),(19, [])]
    , test' 4 "0002.json" [(16, [("Black",6)]),(19, [("Black",1)])]
    , test' 1 "0003.json" [(21, [("Black",9)]),(24, [])]
    , test' 2 "0003.json" [(21, [("Black",9)]),(24, [])]
    , test' 3 "0003.json" [(21, [("Black",9)]),(24, [])]
    , test' 4 "0003.json" [(21, [("Black",9)]),(24, [])]
    ]
  , testGroup "queue"
    [
      test' 1 "0006.json" [(37, [("Black",9)]),(40, [])]
    , test' 2 "0006.json" [(37, [("Black",8)]),(40, [])]
    , test' 3 "0006.json" [(37, [("Black",7)]),(40, [])]
    , test' 4 "0006.json" [(37, [("Black",6)]),(40, [("Black",1)])]
    , test' 5 "0006.json" [(37, [("Black",5)]),(40, [("Black",1)])]
    , test' 6 "0006.json" [(37, [("Black",4)]),(40, [("Black",1)])]
    , test' 7 "0006.json" [(37, [("Black",3)]),(40, [("Black",2)])]
    , test' 8 "0006.json" [(37, [("Black",2)]),(40, [("Black",2)])]
    , test' 1 "0007.json" [(42, [("Black",9)]),(45, [])]
    , test' 2 "0007.json" [(42, [("Black",9)]),(45, [])]
    , test' 3 "0007.json" [(42, [("Black",9)]),(45, [])]
    , test' 4 "0007.json" [(42, [("Black",9)]),(45, [])]
    ]
  ]
  where testRaw node steps file right = testOneNodeResourcesRaw node steps ("ours/101-objects/" </> file) right
        test node steps file right = testOneNodeResources node steps ("ours/101-objects/" </> file) right
        testRaw' steps file noderights = testNodeResourcesAndRegistersRaw steps ("ours/101-objects/" </> file) noderights []
        test' steps file = testNodeResources steps ("ours/101-objects/" </> file)

connections101Tests = testGroup "101-connections"
  [
    testGroup "state"
    [
      testGroup "intervals"
      [
        test' 1 "0040.json" [(312, []),(314, [])]
      , test' 2 "0040.json" [(312, []),(314, [])]
      , test' 3 "0040.json" [(312, [("Black",1)]),(314, [])]
      ]
    , testGroup "modifiers"
      [
        test' 1 "0019.json" [(148, []),(150, [("Black",1)])]
      , test' 2 "0019.json" [(148, [("Black",1)]),(150, [("Black",2)])]
      , test' 3 "0019.json" [(148, [("Black",1)]),(150, [("Black",4)])]
      , test' 1 "0020.json" [(154, []),(156, [])]
      , test' 2 "0020.json" [(154, [("Black",1)]),(156, [])]
      , test' 3 "0020.json" [(154, [("Black",1)]),(156, [("Black",5)])]
      , test' 1 "0021.json" [(161, []),(163, [("Black",10)])]
      , test' 2 "0021.json" [(161, [("Black",1)]),(163, [("Black",20)])]
      , test' 3 "0021.json" [(161, [("Black",1)]),(163, [("Black",29)])]
      , test' 1 "0022.json" [(168, []),(170, [("Black",1)])]
      , test' 2 "0022.json" [(168, [("Black",1)]),(170, [("Black",2)])]
      , test' 3 "0022.json" [(168, [("Black",1)]),(170, [("Black",2)])]
      , test' 4 "0022.json" [(168, [("Black",2)]),(170, [("Black",3)])]
      , test' 1 "0023.json" [(175, []),(177, [])]
      , test' 2 "0023.json" [(175, [("Black",1)]),(177, [])]
      , test' 4 "0023.json" [(175, [("Black",2)]),(177, [])]
      , test' 6 "0023.json" [(175, [("Black",3)]),(177, [])]
      , test' 7 "0023.json" [(175, [("Black",3)]),(177, [("Black",1)])]
      , test' 1 "0041.json" [(319, []),(321, [("Black",1)]),(324, [])]
      , test' 2 "0041.json" [(319, [("Black",1)]),(321, [("Black",2)]),(324, [])]
      , test' 3 "0041.json" [(319, [("Black",1)]),(321, [("Black",4)]),(324, [("Black",1)])]
      , test' 4 "0041.json" [(319, [("Black",2)]),(321, [("Black",4)]),(324, [("Black",1)])]
      , test' 5 "0041.json" [(319, [("Black",2)]),(321, [("Black",7)]),(324, [("Black",1)])]
      ]
    , testGroup "conditions"
      [
        test' 1 "0027.json" [(209, []),(211, [])]
      , test' 2 "0027.json" [(209, []),(211, [])]
      , test' 3 "0027.json" [(209, [("Black",1)]),(211, [])]
      , test' 4 "0027.json" [(209, [("Black",1)]),(211, [("Black",1)])]
      , test' 5 "0027.json" [(209, [("Black",1)]),(211, [("Black",2)])]
      , test' 1 "0028.json" [(216, [("Black",1)]),(218, [("Black",1)])]
      , test' 2 "0028.json" [(216, [("Black",2)]),(218, [("Black",2)])]
      , test' 4 "0028.json" [(216, [("Black",4)]),(218, [("Black",4)])]
      , test' 5 "0028.json" [(216, [("Black",5)]),(218, [("Black",5)])]
      , test' 6 "0028.json" [(216, [("Black",6)]),(218, [("Black",5)])]
      , test' 1 "0032.json" [(244, []),(246, [])]
      , test' 2 "0032.json" [(244, []),(246, [])]
      , test' 3 "0032.json" [(244, [("Black",1)]),(246, [])]
      , test' 4 "0032.json" [(244, [("Black",1)]),(246, [("Black",1)])]
      , test' 5 "0032.json" [(244, [("Black",1)]),(246, [("Black",2)])]
      , test' 6 "0032.json" [(244, [("Black",2)]),(246, [("Black",3)])]
      , test' 1 "0033.json" [(251, [("Black",1)]),(253, [("Black",1)])]
      , test' 2 "0033.json" [(251, [("Black",2)]),(253, [("Black",2)])]
      , test' 5 "0033.json" [(251, [("Black",5)]),(253, [("Black",5)])]
      , test' 6 "0033.json" [(251, [("Black",6)]),(253, [("Black",5)])]
      , test' 1 "0029.json" [(223, [("Black",1)]),(225, [])]
      , test' 2 "0029.json" [(223, [("Black",2)]),(225, [])]
      , test' 3 "0029.json" [(223, [("Black",3)]),(225, [("Black",1)])]
      , test' 4 "0029.json" [(223, [("Black",4)]),(225, [("Black",1)])]
      , test' 1 "0034.json" [(258, [("Black",1)]),(260, [])]
      , test' 2 "0034.json" [(258, [("Black",2)]),(260, [])]
      , test' 3 "0034.json" [(258, [("Black",3)]),(260, [("Black",1)])]
      , test' 4 "0034.json" [(258, [("Black",4)]),(260, [("Black",1)])]
      , test' 1 "0030.json" [(230, [("Black",1)]),(232, [])]
      , test' 3 "0030.json" [(230, [("Black",3)]),(232, [])]
      , test' 4 "0030.json" [(230, [("Black",4)]),(232, [("Black",1)])]
      , test' 1 "0035.json" [(265, [("Black",1)]),(267, [])]         
      , test' 3 "0035.json" [(265, [("Black",3)]),(267, [])]         
      , test' 4 "0035.json" [(265, [("Black",4)]),(267, [("Black",1)])]         
      , test' 1 "0031.json" [(237, [("Black",1)]),(239, [])]
      , test' 3 "0031.json" [(237, [("Black",3)]),(239, [])]
      , test' 4 "0031.json" [(237, [("Black",4)]),(239, [("Black",1)])]
      , test' 6 "0031.json" [(237, [("Black",6)]),(239, [("Black",3)])]
      , test' 7 "0031.json" [(237, [("Black",7)]),(239, [("Black",3)])]
      , test' 1 "0036.json" [(272, [("Black",1)]),(274, [])]
      , test' 3 "0036.json" [(272, [("Black",3)]),(274, [])]
      , test' 4 "0036.json" [(272, [("Black",4)]),(274, [("Black",1)])]
      , test' 6 "0036.json" [(272, [("Black",6)]),(274, [("Black",3)])]
      , test' 7 "0036.json" [(272, [("Black",7)]),(274, [("Black",3)])]
      , test' 1 "0037.json" [(279, [("Black",1)]),(281, [])]
      , test' 2 "0037.json" [(279, [("Black",2)]),(281, [])]
      , test' 6 "0037.json" [(279, [("Black",6)]),(281, [])]
      , test' 7 "0037.json" [(279, [("Black",7)]),(281, [])]
      , test' 1 "0039.json" [(305, [("Black",1)]),(307, [])]
      , test' 2 "0039.json" [(305, [("Black",2)]),(307, [])]
      , test' 6 "0039.json" [(305, [("Black",6)]),(307, [])]
      , test' 7 "0039.json" [(305, [("Black",7)]),(307, [("Black",1)])]
      ]
    , testGroup "triggers"
      [
        test' 4 "0040.json" [(312, [("Black",1)]),(314, [("Black",1)])]
      , test' 5 "0040.json" [(312, [("Black",1)]),(314, [("Black",1)])]
      , test' 6 "0040.json" [(312, [("Black",2)]),(314, [("Black",1)])]
      , test' 7 "0040.json" [(312, [("Black",2)]),(314, [("Black",2)])]
      , test' 1 "0042.json" [(330, []),(332, [])]
      , test' 2 "0042.json" [(330, []),(332, [])]
      , test' 3 "0042.json" [(330, [("Black",1)]),(332, [])]
      , test' 4 "0042.json" [(330, [("Black",1)]),(332, [("Black",5)])]
      , test' 1 "0043.json" [(337, []),(339, [("Black",1)])]
      , test' 2 "0043.json" [(337, []),(339, [("Black",2)])]
      , test' 3 "0043.json" [(337, [("Black",1)]),(339, [("Black",3)])]
      , test' 4 "0043.json" [(337, [("Black",1)]),(339, [("Black",4)])]
      , test' 1 "0044.json" [(346, [("Black",9)]),(348, []),(350, [("Black",1)])]
      , test' 2 "0044.json" [(346, [("Black",8)]),(348, []),(350, [("Black",2)])]
      , test' 3 "0044.json" [(346, [("Black",8)]),(348, []),(350, [("Black",3)])]
      , test' 4 "0044.json" [(346, [("Black",7)]),(348, [("Black",3)]),(350, [("Black",1)])]
      , test' 1 "0045.json" [(358, []),(356, []),(362, []),(366, [])]
      , test' 2 "0045.json" [(358, []),(356, []),(362, []),(366, [])]
      , test' 3 "0045.json" [(358, []),(356, [("Black",1)]),(362, []),(366, [])]
      , test' 4 "0045.json" [(358, [("Black",1)]),(356, [("Black",1)]),(362, [("Black",1)]),(366, [("Black",1)])]
      , test' 5 "0045.json" [(358, [("Black",1)]),(356, [("Black",1)]),(362, [("Black",1)]),(366, [("Black",1)])]
      , test' 1 "0047.json" [(386, [("Black",1)]),(390, []),(392, [])]
      , test' 2 "0047.json" [(386, [("Black",5)]),(390, []),(392, [("Black",1)])]
      , test' 6 "0047.json" [(386, [("Black",19)]),(390, []),(392, [("Black",5)])]
      , test' 7 "0047.json" [(386, [("Black",21)]),(390, []),(392, [("Black",6)])]
      , test' 8 "0047.json" [(386, [("Black",4)]),(390, [("Black",1)]),(392, [("Black",7)])]
      , test' 9 "0047.json" [(386, [("Black",7)]),(390, [("Black",1)]),(392, [])]
      , test' 1 "0046.json" [(374, []),(377, []),(378, [])]
      , test' 3 "0046.json" [(374, [("Black",1)]),(377, []),(378, [])]
      , test' 4 "0046.json" [(374, [("Black",1)]),(377, []),(378, [])]
      , test' 6 "0046.json" [(374, [("Black",2)]),(377, []),(378, [])]
      , test' 7 "0046.json" [(374, [("Black",2)]),(377, [("Black",1)]),(378, [])]
      , test' 9 "0046.json" [(374, [("Black",3)]),(377, [("Black",3)]),(378, [])]
      , test' 10 "0046.json" [(374, [("Black",3)]),(377, [("Black",1)]),(378, [("Black",3)])]
      ]
    , testGroup "modifiers and triggers"
      [
        test' 1 "0062.json" [(461, [("Black",1)])]
      , test' 2 "0062.json" [(461, [("Black",4)])]
      , test' 3 "0062.json" [(461, [("Black",13)])]
      , test' 4 "0062.json" [(461, [("Black",40)])]
      ]
    , testGroup "end"
      [
        testEnded' 1 "0006.json" 64 False []
      , testEnded' 2 "0006.json" 64 False []
      , testEnded' 1 "0006.json" 64 True [75]
      , testEnded' 1 "0006.json" 64 True [75]
      , testEnded' 2 "0006.json" 64 True [75]
      , testEnded' 3 "0006.json" 64 True [75]
      , testEnded' 1 "0007.json" 67 False [77]
      , testEnded' 2 "0007.json" 67 True [77]
      , testEnded' 1 "0008.json" 70 False [79]
      , testEnded' 2 "0008.json" 70 True [79]
      , testEnded' 3 "0008.json" 70 True [79]
      , testEnded' 1 "0009.json" 73 True [81]
      , testEnded' 1 "0009.json" 73 False []
      ]
    , testGroup "registers"
      [
        test'' 1 "0000.json" [(7,[("Black",1)]),(9,[("Black",1)]),(11,[("Black",1)])] [(13,3)]
      , test'' 2 "0000.json" [(7,[("Black",2)]),(9,[("Black",2)]),(11,[("Black",2)])] [(13,6)]
      , test'' 1 "0002.json" [(23,[("Black",1)]),(28,[("Black",4)]),(36,[])] [(32,1)]
      , test'' 2 "0002.json" [(23,[("Black",4)]),(28,[("Black",6)]),(36,[("Black",1)])] [(32,2)]
      , test'' 3 "0002.json" [(23,[("Black",5)]),(28,[("Black",2)]),(36,[("Black",2)])] [(32,1)]
      , test'' 1 "0003.json" [(40,[("Black",1)]),(45,[("Black",4)]),(53,[])] [(49,0)]
      , test'' 2 "0003.json" [(40,[("Black",4)]),(45,[("Black",6)]),(53,[])] [(49,1)]
      , test'' 3 "0003.json" [(40,[("Black",4)]),(45,[("Black",4)]),(53,[("Black",1)])] [(49,1)]
      , test' 1 "0001.json" [(18,[("Black",2)])]
      , test' 2 "0001.json" [(18,[("Black",4)])]
      ]
    ]
  ]
  where testRaw node steps file right = testOneNodeResourcesRaw node steps ("ours/101-connections/" </> file) right
        test node steps file right = testOneNodeResources node steps ("ours/101-connections/" </> file) right
        testRaw' steps file noderights = testNodeResourcesAndRegistersRaw steps ("ours/101-connections/" </> file) noderights []
        test' steps file = testNodeResources steps ("ours/101-connections/" </> file)
        testEnded' steps file node isEnded activated = testEnded steps ("ours/101-connections/" </> file) node isEnded activated
        testRaw'' steps file = testNodeResourcesAndRegistersRaw steps ("ours/101-connections/" </> file)
        test'' steps file = testNodeResourcesAndRegisters steps ("ours/101-connections/" </> file)
        
          
spdTests = testGroup "SourcePoolDrain"
  [ testCase "static" $
    noderes 1       (exSourcePoolDrain Passive   Passive   (Pushing PushAny) Passive   3 7) @?= Just [("life",1)]
  ,testCase "static run" $
    noderes 1 (run' (exSourcePoolDrain Passive   Passive   (Pushing PushAny) Passive   3 7)) @?= Just [("life",1)]
  ,testCase "source run" $
    noderes 1 (run' (exSourcePoolDrain Automatic Passive   (Pushing PushAny) Passive   3 7)) @?= Just [("life",4)]
  ,testCase "drain run" $
    noderes 1 (run' (exSourcePoolDrain Passive   Passive   (Pushing PushAny) Automatic 3 7)) @?= Just []
  ,testCase "pool run" $
    noderes 1 (run' (exSourcePoolDrain Passive   Automatic (Pushing PushAny) Passive   3 7)) @?= Just []
  ,testCase "pool run" $
    noderes 1 (run' (exSourcePoolDrain Passive   Automatic (Pulling PullAny) Passive   3 7)) @?= Just [("life",4)]
  ]

miscTests = testGroup "discord-delay-bug" [
    testNodeResources 1 "xmls/strange-loop-bug-delay.json" [(115,[]),(120,[])]
  , testNodeResources 2 "xmls/strange-loop-bug-delay.json" [(115,[("Black",1)]),(120,[])]
  , testNodeResources 3 "xmls/strange-loop-bug-delay.json" [(115,[]),(120,[("Black",1)])]
  , testNodeResources 1 "xmls/strange-loop-bug-delay-2.json" [(115,[]),(120,[])]
  , testNodeResources 2 "xmls/strange-loop-bug-delay-2.json" [(115,[]),(120,[])]
  , testNodeResources 3 "xmls/strange-loop-bug-delay-2.json" [(115,[("Black",1)]),(120,[])]
  , testNodeResources 4 "xmls/strange-loop-bug-delay-2.json" [(115,[]),(120,[("Black",1)])]
  ]

testRF s = isJust (parseRF s) @? T.unpack s
testSF s = isJust (parseSF s) @? T.unpack s

rfTests = testGroup "ResourceFormulas"
  [ testCase "static" $ testRF "1"
  , testCase "simple" $ testRF "D5"
  , testCase "medium" $ testRF "D5+2"
  , testCase "complex" $ testRF "1+D5*10%"
  ]

sfTests = testGroup "StateFormulas"
  [ testCase "simple" $ testSF "+1"
  , testCase "simple" $ testSF "-1"
  , testCase "i" $ testSF "+1i"
  , testCase "%" $ testSF "-1%"
  , testCase "*" $ testSF "*"
  , testCase "!" $ testSF "!"
  , testCase ">=" $ testSF ">=3"
  , testCase "range" $ testSF "3..5"
  ]

-- TODO Test that these converge to the right probabilities
--  map length $ group $ sort $ map (\x -> fst $ weightedSample [(1,5),(2,1),(3,4)] (mkStdGen x)) [0..100000]
-- weightedShuffle [] (mkStdGen 1)
-- map length $ group $ sort $ map (\x -> head $ map fst $ fst $ weightedShuffle [(1,5),(2,1),(3,4)] (mkStdGen x)) [0..100000]
