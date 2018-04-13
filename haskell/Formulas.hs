{-# LANGUAGE TupleSections #-}

module Formulas where

import qualified Data.Map as M

import Control.Monad.State
import System.Random


quizNames =    quiz "scale formula" "scale name" formulaNamePairs
quizFormulas = quiz "scale name" "scale formula" nameFormulaPairs

quiz :: (Show a, Show b) => String -> String -> [(a,b)] -> Int -> IO ()
quiz keyType valType kvList seed = loop rands
  where rands = randomRs (0, length kvList - 1) (mkStdGen seed) :: [Int]
        loop rands = do
          let kv = kvList !! head rands
          putStrLn $ "\n\n" ++ keyType ++ ": " ++ (show $ fst kv)
          putStrLn $ valType ++ "?"
          x <- getChar
          case x of 'q' -> return ()
                    _   -> do putStrLn $ show $ snd kv
                              loop $ tail rands

formulaNamePairs :: [([Int], [String])]
formulaNamePairs = map (\(a,b) -> (b,a)) nameSetFormulaPairs

nameFormulaPairs :: [(String, [Int])]
nameFormulaPairs = concatMap (\(a,b) -> map (,b) a) nameSetFormulaPairs

nameSetFormulaPairs :: [( [String], [Int] )]
nameSetFormulaPairs = 
  [ (["maj"],[0,2,4,5,7,9,11])
  , (["dor"],[0,2,3,5,7,9,10])
  , (["phr"],[0,1,3,5,7,8,10])
  , (["lyd"],[0,2,4,6,7,9,11])
  , (["mix"],[0,2,4,5,7,9,10])
  , (["min"],[0,2,3,5,7,8,10])
  , (["loc"],[0,1,3,5,6,8,10])

  , (["dor 7","maj 3"],[0,2,3,5,7,9,11])
  , (["phr 6","dor 2"],[0,1,3,5,7,9,10])
  , (["lyd 5"],[0,2,4,6,8,9,11])
  , (["mix 4","lyd 7"],[0,2,4,6,7,9,10])
  , (["min 3","mix 6"],[0,2,4,5,7,8,10])
  , (["loc 2","min 5"],[0,2,3,5,6,8,10])
  , (["loc 4"],[0,1,3,4,6,8,10])

  , (["min 7"],[0,2,3,5,7,8,11])
  , (["loc 6"],[0,1,3,5,6,9,10])
  , (["maj 5"],[0,2,4,5,8,9,11])
  , (["dor 4"],[0,2,3,6,7,9,10])
  , (["phr 3"],[0,1,4,5,7,8,10])
  , (["lyd 2"],[0,3,4,6,7,9,11])
  , (["loc 47"],[0,1,3,4,6,8,9])

  , (["maj 6"],[0,2,4,5,7,8,11])
  , (["dor 5"],[0,2,3,5,6,9,10])
  , (["phr 4"],[0,1,3,4,7,8,10])
  , (["lyd 3"],[0,2,3,6,7,9,11])
  , (["mix 2"],[0,1,4,5,7,9,10])
  , (["lyd 25"],[0,3,4,6,8,9,11])
  , (["loc 7"],[0,1,3,5,6,8,9])

  , (["whole"],[0,2,4,6,8,10])
  , (["dim up"],[0,2,3,5,6,8,9,11])
  , (["dim down"],[0,1,3,4,6,7,9,10])
  , (["aug up"],[0,3,4,7,8,11])
  , (["aug down"],[0,1,4,5,8,9])
  ]
