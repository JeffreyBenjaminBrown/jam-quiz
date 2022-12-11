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


-- -- | * Some scale families of various edos
--

familiar a b = wellBehavedScaleFamilies a b (45,255) (245,455) (345,655)

nice6tone41edoScales = familiar 41 6
nice7tone41edoScales = familiar 41 7
nice8tone41edoScales = familiar 41 8
nice9tone41edoScales = familiar 41 9

nice6tone17edoScales = familiar 17 6
nice7tone17edoScales = familiar 17 7
nice8tone17edoScales = familiar 17 8
nice9tone17edoScales = familiar 17 9

nice6tone12edoScales = familiar 12 6
nice7tone12edoScales = familiar 12 7
nice8tone12edoScales = familiar 12 8
nice9tone12edoScales = familiar 12 9
