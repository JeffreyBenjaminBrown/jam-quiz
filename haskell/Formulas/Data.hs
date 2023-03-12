{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Formulas.Data where

import qualified Data.List as L


-- | = Data
formulaNamePairs :: [([Int], [String])]
formulaNamePairs = map (\(a,b) -> (b,a)) nameSetFormulaPairs

nameFormulaPairs :: [(String, [Int])]
nameFormulaPairs = concatMap (\(a,b) -> map (,b) a) nameSetFormulaPairs

nameVariantPairs :: [(String, [String])]
nameVariantPairs = map k variantFamilies where
  k :: [String] -> (String, [String])
  k vs@(variant:_) = (head $ words variant, concatMap (tail . words) vs)

variantFamilies :: [[String]]
variantFamilies = families where
  names = concatMap fst nameSetFormulaPairs
  variants = filter (\a -> length (words a) > 1) names
  families = foldl k [] $ L.sort variants where
    k :: [[String]] -> String -> [[String]]
    k [] vart = [[vart]]
    k f@(fam:fams) vart =
      if head (words vart) == head (words $ head fam)
      then (vart : fam) : fams
      else [vart] : f

nameSetFormulaPairs :: [( [String], [Int] )]
nameSetFormulaPairs = concat nameSetFormulaFamilies

nameSetFormulaFamilies :: [[( [String], [Int] )]]
nameSetFormulaFamilies =
  [ [ (["maj"],[0,2,4,5,7,9,11])
    , (["dor"],[0,2,3,5,7,9,10])
    , (["phr"],[0,1,3,5,7,8,10])
    , (["lyd"],[0,2,4,6,7,9,11])
    , (["mix"],[0,2,4,5,7,9,10])
    , (["aeo"],[0,2,3,5,7,8,10])
    , (["loc"],[0,1,3,5,6,8,10]) ]

  , [ (["dor 7","maj 3"],[0,2,3,5,7,9,11])
    , (["phr 6","dor 2"],[0,1,3,5,7,9,10])
    , (["lyd 5"        ],[0,2,4,6,8,9,11])
    , (["mix 4","lyd 7"],[0,2,4,6,7,9,10])
    , (["aeo 3","mix 6"],[0,2,4,5,7,8,10])
    , (["loc 2","aeo 5"],[0,2,3,5,6,8,10])
    , ([        "loc 4"],[0,1,3,4,6,8,10]) ]

  , [ (["aeo 7"],[0,2,3,5,7,8,11])
    , (["loc 6"],[0,1,3,5,6,9,10])
    , (["maj 5"],[0,2,4,5,8,9,11])
    , (["dor 4"],[0,2,3,6,7,9,10])
    , (["phr 3"],[0,1,4,5,7,8,10])
    , (["lyd 2"],[0,3,4,6,7,9,11])
    , (["loc 47"],[0,1,3,4,6,8,9]) ]

  , [ (["maj 6"],[0,2,4,5,7,8,11])
    , (["dor 5"],[0,2,3,5,6,9,10])
    , (["phr 4"],[0,1,3,4,7,8,10])
    , (["lyd 3"],[0,2,3,6,7,9,11])
    , (["mix 2"],[0,1,4,5,7,9,10])
    , (["lyd 25"],[0,3,4,6,8,9,11])
    , (["loc 7"],[0,1,3,5,6,8,9]) ]

  , [ (["whole"],[0,2,4,6,8,10]) ]
  , [ (["dim up"],[0,2,3,5,6,8,9,11])
    , (["dim down"],[0,1,3,4,6,7,9,10]) ]
  , [ (["aug up"],[0,3,4,7,8,11])
    , (["aug down"],[0,1,4,5,8,9]) ]
  ]
