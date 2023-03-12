module EdoMath where

import Quiz
import Util


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
