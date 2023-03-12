-- TODO : given scale, degree, what's the other scale
--      : (hard) given 2 root-scale pairs, which notes in common
{-# LANGUAGE TupleSections #-}
module Formulas where

import           Control.Monad.State
import qualified Data.Map as M
import           System.Random

import Formulas.Data


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

quizKVList :: (Show a, Show b)
           => String -> String -> [(a,b)] -> Int -> IO ()
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
    let (_,kvl) = kvll !!! a
        (n,start) = kvl !!! b
        (m,_) = kvl !!! c
        m' = mod (m + n) (length kvl)
        finish = kvl !! m'
    case m of 0 -> loop moreRands -- try again
              _ -> quizKVPair
                   ("Starting from " ++ keyType)
                   ("What is the " ++ valType ++ " " ++ show m
                    ++ " places (a \"" ++ show (m+1) ++ "th\") higher")
                   (start,finish)
                   (loop moreRands)
