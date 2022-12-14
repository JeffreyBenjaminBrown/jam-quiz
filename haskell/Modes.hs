{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Modes where

import qualified Data.List as L
import           Data.Set (Set)
import qualified Data.Set as S

import Util


wellBehavedScaleFamilies
  :: Edo
  -> Int -- ^ ^ number of pitches
  -> (Float, Float) -- ^ range of seconds in cents
  -> (Float, Float) -- ^ range of thirds  in cents
  -> (Float, Float) -- ^ range of fourths in cents
  -> [[Int]] -- ^ Each @[Int]@ in this is a mode representating a family distinct from all the others
wellBehavedScaleFamilies edo a (minJumpCents, maxJumpCents) b c =
  let minJump = minInCents_toMinInEdo edo minJumpCents
      maxJump = maxInCents_toMaxInEdo edo maxJumpCents
  in wellBehavedScaleFamilies' edo a (minJump, maxJump) b c

wellBehavedScaleFamilies' :: Edo
                          -> Int -- ^ ^ number of pitches
                          -> (Int, Int) -- ^ range of seconds, in Edo steps
                          -> (Float, Float) -- ^ range of thirds  in cents
                          -> (Float, Float) -- ^ range of fourths in cents
                          -> [[Int]]
wellBehavedScaleFamilies' edo size
    (minJump, maxJump) (minThird, maxThird) (minFourth, maxFourth) =
  equivalents edo
  $ filter (nthDifferencesIn 3 (minFourth, maxFourth) edo)
  $ filter (nthDifferencesIn 2 (minThird,  maxThird) edo)
  $ monoAscendingFromZero edo minJump maxJump size

-- | `nthDifferencesIn` generalizes `thirdsIn`.
-- Rather than fixing the difference at 2,
-- it lets that difference be any number `n`.
-- In a @thirds_in (x,y)@ scale,
-- the thirds are all between @x@ and @y@ cents.
--
-- For thirds, I like (x,y) = (245,455).
-- Those will admit every septimal minor and major third,
-- even in an edo where the septimal minor third is around 2250c
-- (which makes it equal to the septimal major second).
-- The true 9:7 is around 435c, and the true 7:6 around 267c,
-- so the above gives about 20c of leeway to each.
-- Also it has the symmetry of giving 55c more on either side of (300,400),
-- i.e. the (floating-point-error-free)
-- range of thirds in well-behaved scales in 12-edo.
--
-- For fourths, I'm trying (350,650).
-- That admits both tritones in 41-edo,
-- as well as the major third,
-- which I need to admit to include the harmonic minor and major scales.

nthDifferencesIn :: Int -> (Float, Float) -> Edo -> [Int] -> Bool
nthDifferencesIn n (x,y) edo scale
  | length scale < n+1 = False
  | otherwise = let
      ext = scale ++ map (+edo) (take n scale)
        -- Extended scale, 2 degrees into the next octave.
        -- For instance, [0,2,4,5,7,9,11,12,14] for major in 12-edo.
      rotated = drop n scale ++ map (+edo) (take n scale)
        -- Continuing that example, [4,5,7,9,11,12,14,16,17]
      intervals = zipWith (-) rotated ext
      intervalsInCents = map f intervals where
        f :: Int -> Float
        f i = (fromIntegral i / fromIntegral edo) * 1200
      isGoodInterval :: Float -> Bool
      isGoodInterval f = f > x && f < y
      in all isGoodInterval intervalsInCents

-- | All monotonic ascending *circular* series of length @size@,
-- starting at 0,
-- using the integers modulo @top@,
-- such that every pair of adjacent members
-- differs by at least @minJump@ and at most @maxJump@,
-- including the implicit jump from the last element
-- to @top@.
--
-- TODO: This could be simpler:
-- https://stackoverflow.com/a/74758782/916142
monoAscendingFromZero :: Int -> Int -> Int -> Int -> [[Int]]
monoAscendingFromZero top minJump maxJump size =
  map reverse $
  filter (\list -> top - head list <= maxJump) $
    -- This ensures @maxJump@ applies even to the last jump,
    -- where the scale wraps back on itself.
  incrementNTimes top (size-1) $ [[0]]
  where
    incrementNTimes :: Int -> Int -> [[Int]] -> [[Int]]
    incrementNTimes top 0 lists = lists
    incrementNTimes top n lists = let
      x :: [[Int]]
      x = concatMap (increments top) lists
      in incrementNTimes top (n-1) x

    -- | All the ways of prepending a bigger element to the input list.
    -- Assumes the input list is in descending order.
    increments :: Int -> [Int] -> [[Int]]
    increments top (a:as) =
      [ b:a:as
      | b <- [a + minJump ..
               min (top - minJump) (a + maxJump) ] ]

allTriads :: Edo -> [[Int]]
allTriads edo = [ [0,a,b]
                | a <- [1 .. edo-1],
                  b <- [a+1 .. edo-1] ]

equivalents :: Edo -> [[Int]] -> [[Int]]
equivalents _ [] = []
equivalents edo scales =
  S.toList $ S.fromList $ map (minimal_mode edo) scales

minimal_mode :: Edo -> [Int] -> [Int]
minimal_mode _ [] = []
minimal_mode edo scale =
  head $ L.sort $ allModes edo scale

scaleToNewEdo_many :: Edo -> Edo -> [Int] -> [[Int]]
scaleToNewEdo_many from to scale = let
  ms :: [[Int]] =
    allModes from scale
  in equivalents to $ map (scaleToNewEdo from to) ms

scaleToNewEdo :: Edo -> Edo -> [Int] -> [Int]
scaleToNewEdo from to = map go where
  go :: Int -> Int
  go = round . (* fromIntegral to) . (/ fromIntegral from) . fromIntegral

allModes :: Edo -> [Int] -> [[Int]]
allModes edo scale =
  [ mode edo scale i
  | i <- [0..length scale - 1] ]

minInCents_toMinInEdo :: Edo -> Float -> Int
minInCents_toMinInEdo edo cents =
  ceiling $ cents * fromIntegral edo / 1200

maxInCents_toMaxInEdo :: Edo -> Float -> Int
maxInCents_toMaxInEdo edo cents =
  floor $ cents * fromIntegral edo / 1200

mode :: Edo
     -> [Int]
     -> Int -- ^ PITFALL: 0-indexed. So for example,
            -- @mode 12 major 2@ = phrygian, not dorian.
     -> [Int]
mode _ [] _ = []
mode edo scale index = let
  rotated = drop index scale
            ++ map (+edo) (take index scale)
  first = head rotated
  in map (+ (- first)) rotated
