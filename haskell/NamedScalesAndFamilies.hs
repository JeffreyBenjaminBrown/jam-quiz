module NamedScalesAndFamilies where

import qualified Data.List as L

import IntervalScores
import Modes
import Util


-- | * Some 41-edo scales

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


-- | * Some scale families of various edos,
-- with their sizes in comments.

familiar edo size =
  L.sortOn scaleScore41 $
  wellBehavedScaleFamilies edo size
  (45,355) (245,455) (345,755)

familiarBut2ndsGT75 edo size =
  wellBehavedScaleFamilies edo size
  (75,355) (245,455) (345,755)


-- | ** I give these names so that they won't be recomputed.

nice6tone41edoFamilies = familiar 41 6 -- 280
nice7tone41edoFamilies = familiar 41 7 -- 5428

nice8tone41edoFamilies = familiar 41 8 -- halting?
nice9tone41edoFamilies = familiar 41 9 -- halting?

nice6tone34edoFamilies = familiar 34 6 -- 34
nice7tone34edoFamilies = familiar 34 7 -- 1899
nice8tone34edoFamilies = familiar 34 8 -- 1772
nice9tone34edoFamilies = familiar 34 9 -- 74

nice6tone31edoFamilies = familiar 31 6 -- 36
nice7tone31edoFamilies = familiar 31 7 -- 705
nice8tone31edoFamilies = familiar 31 8 -- 148
nice9tone31edoFamilies = familiar 31 9 -- 0

nice6tone24edoFamilies = familiar 24 6 -- 23
nice7tone24edoFamilies = familiar 24 7 -- 196
nice8tone24edoFamilies = familiar 24 8 -- 28
nice9tone24edoFamilies = familiar 24 9 -- 0

nice6tone22edoFamilies = familiar 22 6 -- 23
nice7tone22edoFamilies = familiar 22 7 -- 196
nice8tone22edoFamilies = familiar 22 8 -- 28
nice9tone22edoFamilies = familiar 22 9 -- 0

nice6tone19edoFamilies = familiar 19 6 -- 16
nice7tone19edoFamilies = familiar 19 7 -- 160
nice8tone19edoFamilies = familiar 19 8 -- 60
nice9tone19edoFamilies = familiar 19 9 -- 2

nice6tone17edoFamilies = familiar 17 6 -- 5
nice7tone17edoFamilies = familiar 17 7 -- 39
nice8tone17edoFamilies = familiar 17 8 -- 5
nice9tone17edoFamilies = familiar 17 9 -- 0

nice6tone17edoFamilies' = familiarBut2ndsGT75 17 6 -- 3
nice7tone17edoFamilies' = familiarBut2ndsGT75 17 7 -- 9
nice8tone17edoFamilies' = familiarBut2ndsGT75 17 8 -- 1
nice9tone17edoFamilies' = familiarBut2ndsGT75 17 9 -- 0

nice6tone15edoFamilies = familiar 15 6 -- 2
nice7tone15edoFamilies = familiar 15 7 -- 3
nice8tone15edoFamilies = familiar 15 8 -- 0
nice9tone15edoFamilies = familiar 15 9 -- 0

nice6tone14edoFamilies = familiar 14 6 -- 4
nice7tone14edoFamilies = familiar 14 7 -- 23
nice8tone14edoFamilies = familiar 14 8 -- 3
nice9tone14edoFamilies = familiar 14 9 -- 0

nice6tone13edoFamilies = familiar 13 6 -- 0
nice7tone13edoFamilies = familiar 13 7 -- 3
nice8tone13edoFamilies = familiar 13 8 -- 3
nice9tone13edoFamilies = familiar 13 9 -- 0

nice6tone12edoFamilies = familiar 12 6 -- 2 (whole, aug)
nice7tone12edoFamilies = familiar 12 7 -- 4 (diatonic, melodic, harm+, harm-)
nice8tone12edoFamilies = familiar 12 8 -- 1 (dim)
nice9tone12edoFamilies = familiar 12 9 -- 0

nice6tone11edoFamilies = familiar 11 6 -- 2
nice7tone11edoFamilies = familiar 11 7 -- 1
nice8tone11edoFamilies = familiar 11 8 -- 0
nice9tone11edoFamilies = familiar 11 9 -- 0

-- familiar 10 _ is empty!

nice9tone9edoFamilies = familiar 9 9 -- 1
nice8tone9edoFamilies = familiar 9 8 -- 1
nice7tone9edoFamilies = familiar 9 7 -- 2
nice6tone9edoFamilies = familiar 9 6 -- 1

nice8tone8edoFamilies = familiar 8 8 -- 1
nice7tone8edoFamilies = familiar 8 7 -- 1
nice6tone8edoFamilies = familiar 8 6 -- 2

nice7tone7edoFamilies = familiar 7 7 -- 1
nice6tone7edoFamilies = familiar 7 6 -- 0


-- | * Some scales from those families

-- | A way to print the most harmonic scale from each family.
modesOfMostHarmonicTranslation :: Edo -> Edo -> [Int] -> [[Int]]
modesOfMostHarmonicTranslation from to scale = let
  translations :: [[Int]]
  translations = scaleToNewEdo_many from to scale
  scoredTranslations :: [(Float, [Int])]
  scoredTranslations = map (\a -> (scaleScore41 a,a)) translations
  in allModes to $ snd
     ( L.sortOn fst scoredTranslations !! 0 )


-- | ** 15-edo

edo15size6_0 = nice6tone15edoFamilies !! 0
edo15size6_1 = nice6tone15edoFamilies !! 1

edo15size7_0 = nice7tone15edoFamilies !! 0
edo15size7_1 = nice7tone15edoFamilies !! 1
edo15size7_2 = nice7tone15edoFamilies !! 2
-- Shortening the two 25\41 intervals by 1\41
-- yields this wonderful family:
-- [0,6,11,17,24,30,35]
-- [0,5,11,18,24,29,35]
-- [0,6,13,19,24,30,36]
-- [0,7,13,18,24,30,35]
-- [0,6,11,17,23,28,34]
-- [0,5,11,17,22,28,35]
-- [0,6,12,17,23,30,36]
-- albeit by losing two 19s and introducing a 12.


-- | ** 13-edo

edo13size7_0 = nice7tone13edoFamilies !! 0 -- lots of 13s and 19s
edo13size7_1 = nice7tone13edoFamilies !! 1
edo13size7_2 = nice7tone13edoFamilies !! 2

edo13size8_0 = nice8tone13edoFamilies !! 0
edo13size8_1 = nice8tone13edoFamilies !! 1
edo13size8_2 = nice8tone13edoFamilies !! 2


-- | ** 11-edo. Hard to play and nasty.

edo11size6_0 = nice6tone11edoFamilies !! 0
edo11size6_1 = nice6tone11edoFamilies !! 1

edo11size7   = nice7tone11edoFamilies !! 0
