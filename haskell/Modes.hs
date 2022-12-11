{-# LANGUAGE RankNTypes #-}

module Modes where

import qualified Data.List as L
import           Data.Set (Set)
import qualified Data.Set as S


type Edo = Int

allModes :: Edo -> [Int] -> [[Int]]
allModes edo scale =
  [ mode edo scale i
  | i <- [0..length scale - 1] ]

-- | In a "well-behaved" scale, the thirds are all between 245c and 455c.
-- Those are chosen to admit every septimal minor and major third,
-- even in an edo where the septimal minor third is around 2250c
-- (which makes it equal to the septimal major second).
-- (The true 9:7 is around 435c, and the true 7:6 around 267c,
-- so this gives about 20c of leeway to each.)
well_behaved :: Edo -> [Int] -> Bool
well_behaved edo scale
  | length scale < 3 = False
  | otherwise = let
      ext = scale ++ map (+edo) (take 2 scale)
        -- Extended scale, 2 degrees into the next octave.
        -- For instance, [0,2,4,5,7,9,11,12,14] for major in 12-edo.
      rotated = drop 2 scale ++ map (+edo) (take 2 scale)
        -- Continuing that example, [4,5,7,9,11,12,14,16,17]
      thirds = zipWith (-) rotated ext
      thirdsInCents = map f thirds where
        f :: Int -> Float
        f i = (fromIntegral i / fromIntegral edo) * 1200
      isGoodThird :: Float -> Bool
      isGoodThird f = f > 245 && f < 455
      in all isGoodThird thirdsInCents

equivalents :: Edo -> [[Int]] -> [[Int]]
equivalents _ [] = []
equivalents edo scales =
  S.toList $ S.fromList $ map (minimal_mode edo) scales

minimal_mode :: Edo -> [Int] -> [Int]
minimal_mode _ [] = []
minimal_mode edo scale =
  head $ L.sort $ allModes edo scale

mode :: Edo
     -> [Int]
     -> Int -- ^ PITFALL: 0-indexed. So for example,
            -- `mode 12 major 2` = phrygian, not dorian.
     -> [Int]
mode _ [] _ = []
mode edo scale index = let
  rotated = drop index scale
            ++ map (+edo) (take index scale)
  first = head rotated
  in map (+ (- first)) rotated

minor41, minor41_sept, major41, major41_sept, whole, whole_sept, dim_up, dim_up', dim_up_pyth, dim_up_pyth', dim_up_sept, dim_down, dim_down_pyth, dim_down_pyth', dim_down_sept, dim_down_sept', aug_up, aug_up_down6, eq_5, eq_7, eq_8, eq_9  :: [Int]
minor41        = [0,7,11,17,24,28,35]
minor41_sept   = [0,7,9,16,24,26,33]
major41        = [0,7,13,17,24,31,37]
major41_sept   = [0,8,15,17,24,32,39]
whole          = [0,7,14,21,28,35]
whole_sept     = [0,7,15,22,30,37]
dim_up         = [0,7,11,18,22,29,33]
dim_up'        = [0,7,11,18,22,29,33,40]
dim_up_pyth    = [0,7,10,17,20,27,30,37]
dim_up_pyth'   = [0,7,10,17,20,27,30,37,40]
dim_up_sept    = [0,7, 9,16,18,25,27,34]
dim_down       = [0,4,11,15,22,26,33,37]
dim_down_pyth  = [0,3,10,13,20,23,30,33]
dim_down_pyth' = [0,3,10,13,20,23,30,33,40]
dim_down_sept  = [0,2, 9,11,18,20,27,29,36]
dim_down_sept' = [0,2, 9,11,18,20,27,29,36,38]

aug_up         = [0,11,13,24,26,37]
aug_up_down6   = [0,11,13,24,28,37]

eq_5 =  [ round $ i * 41 / 5 | i <- [0..4]]
eq_7 =  [ round $ i * 41 / 7 | i <- [0..6]]
eq_8 =  [ round $ i * 41 / 8 | i <- [0..7]]
eq_9 =  [ round $ i * 41 / 9 | i <- [0..8]]
