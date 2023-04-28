{-# LANGUAGE ScopedTypeVariables #-}

module Quiz where

import System.Random

import Util


{- | Quizzes a user repeatedly from a list of question-answer pairs. -}
quizKVList :: (Show a, Show b)
           => (String, String)
           -> [(a,b)]
           -> Int
           -> IO ()
quizKVList (kType,vType) kvList randomSeed = loop rands
  where rands :: [Int] =
          randomRs (0, length kvList - 1) (mkStdGen randomSeed)
        loop rands = do
          let kv = kvList !! head rands
          quizKVPair (kType, vType) kv $ loop $ tail rands

{- | This is very similar to @quizKVList@ above,
except it takes a @[[(a,b)]]@ instead of a @[(a,b)]@.
Although I'm not (years later) sure what it does,
@quizModesg@ is probably instructive. -}
quizKVRingList :: (Show a, Show b)
               => (String, String)
               -> [[(a,b)]]
               -> Int
               -> IO ()
quizKVRingList (kType, vType) kvll randomSeed = loop rands where
  rands = randomRs (0, 1) (mkStdGen randomSeed) :: [Float]
  loop rands@(a:b:c:moreRands) = do
    let (_,kvl) = kvll !!! a
        (n,start) = kvl !!! b
        (m,_) = kvl !!! c
        m' = mod (m + n) (length kvl)
        finish = kvl !! m'
    case m of 0 -> loop moreRands -- try again
              _ -> quizKVPair
                   ( ("Starting from " ++ kType)
                   , ("What is the " ++ vType ++ " " ++ show m
                      ++ " places (a \"" ++ show (m+1) ++ "th\") higher") )
                   (start,finish)
                   (loop moreRands)

{- | @quizKVPair (kType, vType) kv followAction@
asks a question, then runs @followAction@.
It asks the question by asking, if @kType@ is @k@,
for the user to name the corresponding @vType@.
-}
quizKVPair :: (Show a, Show b)
           => (String, String)
           -> (a,b)
           -> IO ()
           -> IO ()
quizKVPair (kType, vType) (k,v) followAction = do
          putStrLn $ "\n\n" ++ kType ++ ": " ++ show k
          putStrLn $ vType ++ "?"
          x <- getChar
          case x of 'q' -> return ()
                    _   -> do putStrLn $ show v
                              followAction
