{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fwarn-missing-signatures -Wall -Wno-name-shadowing #-}

module Machinations.Misc where
import Data.Aeson
import Data.Char (toLower)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import System.Random
import Data.Bifunctor
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

mkUuid = U.toText <$> U.nextRandom

mkUuid' = first U.toText . random

inv (a,b) = (b,a)

mjsonOptions :: Options
mjsonOptions = defaultOptions{fieldLabelModifier = removeUnderscore
                             ,omitNothingFields = True}

removeUnderscore :: [Char] -> [Char]
removeUnderscore ('_':t) = t
removeUnderscore t = t

prefixOptions :: String -> Options
prefixOptions p = defaultOptions{fieldLabelModifier = prefixed p}
  where prefixed p s = case drop (length p) s of
                         (h:t) -> toLower h : t
                         x -> x

maybeRight :: Either a b -> Maybe b
maybeRight (Left _) = Nothing
maybeRight (Right b) = Just b

findJust :: [Maybe a] -> Maybe a
findJust [] = Nothing
findJust (Just h:t) = Just h
findJust (Nothing:t) = findJust t

weightedSample :: [(a,Int)] -> StdGen -> (Int, StdGen)
weightedSample [] _ = error "Sampling from empty list"
weightedSample l g = first (loop l 0) $ randomR (0, sum (map snd l)-1) g
  where loop ((_,h):t) i n | n < h = i
                           | otherwise = loop t (i+1) (n-h)

-- http://utopia.duth.gr/~pefraimi/research/data/2007EncOfAlg.pdf
weightedShuffle :: [(a,Int)] -> StdGen -> ([(a,Int)], StdGen)
weightedShuffle l g =
  let (g',l') = mapAccumL (\g x@(_,w) ->
                             let (u::Float,g') = random g
                             in (g', (x, - u ** (1 / fromIntegral w)))) g l
  in (map fst $ sortOn snd l', g')

rotate :: Int -> [a] -> [a]
rotate i l = take (length l) $ drop i $ cycle l

-- bimap2 :: ((Any -> Any, Any -> Any) -> Any Any Any -> Any Any Any)
--   -> (Map GraphLabel Float -> Map GraphLabel Float, Float -> Float)
--   -> (Map GraphLabel Double, Double)
--   -> Any
bimap2 :: Bifunctor p => (a1 -> a2 -> b) -> (c1 -> c2 -> d) -> (a1, c1) -> p a2 c2 -> p b d
bimap2 f g a b = uncurry bimap (bimap f g a) b

mapMinimum :: Ord b => Map a b -> Maybe (a, b)
mapMinimum = M.foldlWithKey' (\a k b ->
                                case a of
                                  Nothing -> Just (k,b)
                                  Just (k',b') -> if b < b' then
                                                   Just (k,b) else
                                                   Just (k',b')) Nothing

mapMaximum :: Ord b => Map a b -> Maybe (a, b)
mapMaximum = M.foldlWithKey' (\a k b ->
                                case a of
                                  Nothing -> Just (k,b)
                                  Just (k',b') -> if b > b' then
                                                   Just (k,b) else
                                                   Just (k',b')) Nothing
