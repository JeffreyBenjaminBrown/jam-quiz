module NamedScalesAndFamilies where

import Modes

big = 100


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

familiar edo size = wellBehavedScaleFamilies edo size
                    (45,355) (245,455) (345,755)

nice6tone41edoScales = familiar 41 6 -- 280
nice7tone41edoScales = familiar 41 7 -- 5428
nice8tone41edoScales = familiar 41 8 -- halting?
nice9tone41edoScales = familiar 41 9 -- halting?

nice6tone34edoScales = familiar 34 6 -- 34
nice7tone34edoScales = familiar 34 7 -- 1899
nice8tone34edoScales = familiar 34 8 -- 1772
nice9tone34edoScales = familiar 34 9 -- 74

nice6tone31edoScales = familiar 31 6 -- 36
nice7tone31edoScales = familiar 31 7 -- 705
nice8tone31edoScales = familiar 31 8 -- 148
nice9tone31edoScales = familiar 31 9 -- 0

nice6tone24edoScales = familiar 24 6 -- 23
nice7tone24edoScales = familiar 24 7 -- 196
nice8tone24edoScales = familiar 24 8 -- 28
nice9tone24edoScales = familiar 24 9 -- 0

nice6tone22edoScales = familiar 22 6 -- 23
nice7tone22edoScales = familiar 22 7 -- 196
nice8tone22edoScales = familiar 22 8 -- 28
nice9tone22edoScales = familiar 22 9 -- 0

nice6tone19edoScales = familiar 19 6 -- 16
nice7tone19edoScales = familiar 19 7 -- 160
nice8tone19edoScales = familiar 19 8 -- 60
nice9tone19edoScales = familiar 19 9 -- 2

nice6tone17edoScales = familiar 17 6 -- 5
nice7tone17edoScales = familiar 17 7 -- 39
nice8tone17edoScales = familiar 17 8 -- 5
nice9tone17edoScales = familiar 17 9 -- 0

nice6tone15edoScales = familiar 15 6 -- 2
nice7tone15edoScales = familiar 15 7 -- 3
nice8tone15edoScales = familiar 15 8 -- 0
nice9tone15edoScales = familiar 15 9 -- 0

nice6tone14edoScales = familiar 14 6 -- 4
nice7tone14edoScales = familiar 14 7 -- 23
nice8tone14edoScales = familiar 14 8 -- 3
nice9tone14edoScales = familiar 14 9 -- 0

nice6tone13edoScales = familiar 13 6 -- 0
nice7tone13edoScales = familiar 13 7 -- 3
nice8tone13edoScales = familiar 13 8 -- 3
nice9tone13edoScales = familiar 13 9 -- 0

nice6tone12edoScales = familiar 12 6 -- 2 (whole, aug)
nice7tone12edoScales = familiar 12 7 -- 4 (diatonic, melodic, harm+, harm-)
nice8tone12edoScales = familiar 12 8 -- 1 (dim)
nice9tone12edoScales = familiar 12 9 -- 0
