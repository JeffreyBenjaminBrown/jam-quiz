module EdoMath where

import Quiz
import Util


{- | Assuming `divisor` divides `edo` evenly,
@quizEdoPartitions edo divisor randomSeed@
quizzes the user on where each note sits
relative to the two nearest multiples of @divisor@.

Usage example:
@
quizEdoPartitions 46 23 1
@
-}
quizEdoPartitions :: Edo -> Int -> Int -> IO ()
quizEdoPartitions edo divisor randomSeed = let
  divisorFloor :: Int -> Int
  divisorFloor n = divisor * (n `div` divisor)
  divisorCeiling n = divisorFloor n + divisor
  questions = [ ( n
                , ( show df ++ " + " ++ show (n - df) ++ " = " ++ show n,
                    show n ++ " + " ++ show (dc - n) ++ " = " ++ show dc ) )
              | n <- [1..edo-1]
              , let df = divisorFloor n
                    dc = divisorCeiling n ]
  in quizKVList ("Q: ", "A: ") questions randomSeed

{- | Quizzes the user on complements modulo an edo
-- i.e. n + what = edo. -}
quizEdoComplements :: Edo -> Int -> IO ()
quizEdoComplements edo randomSeed = let
  questions = [ (show edo ++ " minus " ++ show n,
                 edo - n)
              | n <- [1..edo] ]
  in quizKVList ("Q: ", "A: ") questions randomSeed

{- | Quizzes the user on sums modulo an edo. -}
quizEdo :: Edo -> Int -> IO ()
quizEdo e randomSeed = let
  notePairs = [ (a,b)
              | a <- [-e .. e],
                b <- [-e .. e] ]
  questions = [ (show a ++ " + " ++ show b ++ " (mod " ++ show e ++ ")",
                 mod (a + b) e)
              | (a,b) <- notePairs ]
  in quizKVList ("Q: ", "A: ") questions randomSeed
