{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Machinations
import Machinations.Types
import Machinations.Misc
import Machinations.Formulas
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
import Debug.Trace
import Data.Bifunctor

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [object101Tests] -- [object101Tests,rfTests,sfTests,spdTests]

readMachination' :: FilePath -> Machination
readMachination' = fromJust . unsafePerformIO . decodeFileStrict'
noderes n g = resourceStatsByTag <$> nodeResources g (NodeLabel n)
run' g = runNewUpdate $ run g []
run2' = run' . run'
runN' n x = iterate run' x !! n

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
      , 

        test' 1 "0035.json" [(259, [("Black", 1)]), (261, [])]
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
  ]
  where read' x = readMachination' ("ours/101-objects/" </> x)
        testRaw node steps file right =
          testCase (show node <> " x" <> show steps <> " " <> file)
          $ noderes node (runN' steps $ read' file) @?= right
        test node steps file right = testRaw node steps file (Just right)
        testRaw' steps file noderights =
          let r = runN' steps $ read' file
          in testGroup (show (map fst noderights) <> " x" <> show steps <> " " <> file)
             $ map (\(node,right) ->
                      testCase (show node <> " x" <> show steps)
                     $ noderes node r @?= right)
             noderights
        test' steps file =
          testRaw' steps file . map (\(n,r) -> (n,Just r))
          
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
