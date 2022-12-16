module Stats where

import Modes
import Util


sumOfSquaredDegrees
  :: Edo
  -> Int -- ^ the degree to look at
  -> [Int] -- ^ (a mode from) the scale family to score
  -> Int
sumOfSquaredDegrees edo n scale =
  sum $ map (^ 2) $ map (!! n) $ allModes edo scale

hasIntervalsIn
  :: Edo
  -> [Int] -- ^ intervals we care about
  -> [Int] -- ^ (a mode from) the scale family to judge
  -> Bool  -- ^ True if it has at least one of those intervals.
hasIntervalsIn edo avoid scale =
  any (flip elem avoid) $ concat $ allModes edo scale
