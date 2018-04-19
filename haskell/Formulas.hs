-- TODO : given scale, degree, what's the other scale
--      : (hard) given 2 root-scale pairs, which notes in common
{-# LANGUAGE TupleSections #-}
module Formulas where

import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.State
import System.Random


quizNames, quizFormulas, quizVariants, quizModes :: Int -> IO ()
quizNames =    quizKVList "scale formula" "scale name" formulaNamePairs
quizFormulas = quizKVList "scale name" "scale formula" nameFormulaPairs
quizVariants = quizKVList "scale name" "set of variants" nameVariantPairs
quizModes = quizKVRingList "scale" "mode" $ take 4 nameSetFormulaFamilies
  -- take 4 to omit the modes of the symmetric scales, which are obvious

quizKVPair :: (Show a, Show b)
           => String -> String -> (a,b) -> IO ()        -> IO ()
quizKVPair    keyType   valType   kv       followAction =  do
          putStrLn $ "\n\n" ++ keyType ++ ": " ++ (show $ fst kv)
          putStrLn $ valType ++ "?"
          x <- getChar
          case x of 'q' -> return ()
                    _   -> do putStrLn $ show $ snd kv
                              followAction

quizKVList :: (Show a, Show b) => String -> String -> [(a,b)] -> Int -> IO ()
quizKVList keyType valType kvList seed = loop rands
  where rands = randomRs (0, length kvList - 1) (mkStdGen seed) :: [Int]
        loop rands = do
          let kv = kvList !! head rands
          quizKVPair keyType valType kv $ loop $ tail rands

quizKVRingList :: (Show a, Show b)
               => String -> String -> [[(a,b)]] -> Int -> IO ()
quizKVRingList    keyType   valType   kvll         seed = loop rands where
  rands = randomRs (0, 1) (mkStdGen seed) :: [Float]
  loop rands@(a:b:c:moreRands) = do
    let (_,kvl) = choose kvll a
        (n,start) = choose kvl b
        (m,_) = choose kvl c
        m' = mod (m + n) (length kvl)
        finish = kvl !! m'
    case m of 0 -> loop moreRands -- try again
              _ -> quizKVPair
                   ("Starting from " ++ keyType)
                   ("What is the " ++ valType ++ " " ++ show m
                    ++ " places (a \"" ++ show (m+1) ++ "th\") higher")
                   (start,finish)
                   (loop moreRands)

choose :: [a] -> Float -> (Int,a)
choose l f = (n, l !! n) where
  f' = f - fromIntegral (floor f) -- ensures f is in (0,1)
  n = floor $ f' * fromIntegral (length l)


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
    , (["min"],[0,2,3,5,7,8,10])
    , (["loc"],[0,1,3,5,6,8,10]) ]

  , [ (["dor 7","maj 3"],[0,2,3,5,7,9,11])
    , (["phr 6","dor 2"],[0,1,3,5,7,9,10])
    , (["lyd 5"],[0,2,4,6,8,9,11])
    , (["mix 4","lyd 7"],[0,2,4,6,7,9,10])
    , (["min 3","mix 6"],[0,2,4,5,7,8,10])
    , (["loc 2","min 5"],[0,2,3,5,6,8,10])
    , (["loc 4"],[0,1,3,4,6,8,10]) ]

  , [ (["min 7"],[0,2,3,5,7,8,11])
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
