{-# LANGUAGE
ScopedTypeVariables,
TypeApplications #-}

-- | USAGE:
-- The random seed might need changing. Search for "seed" in this file.
-- Most of these functions work in any EDO,
-- but `monome_quizz` is hard-coded to 58-EDO.

module Quiz_58 where

import Data.List as L
import System.Random as R
import System.Random.Internal
import Data.Fixed


-- | TODO: Make this monadic. (Search for "monadic" here:
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html

monome_quiz :: IO ()
monome_quiz =
  go $ random_sequence (0 :: Float, 1) where
  go nums = let
    (col,row,rest) = random_monome_position nums
    in do putStrLn $ "What note is at col "
            ++ show col ++ ", row " ++ show row ++ "?"
          _ <- getChar
          putStrLn $ show $
            monome_position_to_edo_value (col,row)
          go rest

-- quiz_grid_sums 46 4 7  (0,5) (-4,4)
-- quiz_grid_sums 58 15 2 (0,4) (-4,4)
-- quiz_grid_sums 41 8 1  (0,5) (-4,4)
quiz_grid_sums :: Int -> Int      -> Int      -> (Int,Int)    -> (Int,Int) -> IO ()
quiz_grid_sums    edo colInterval rowInterval (minCol,maxCol) (minRow,maxRow) = let
  bound :: Int = edo-1
  go :: [Float] -> IO ()
  go nums = let
    ([n1,n2,n3,n4],rest) :: ([Float],[Float]) = splitAt 4 nums
    i1 = likelyGridInterval n1 n2
      colInterval rowInterval (minCol,maxCol) (minRow,maxRow)
    i2 = likelyGridInterval n3 n4
      colInterval rowInterval (minCol,maxCol) (minRow,maxRow)
    in do putStrLn (  "What is " ++ show i1 ++
                      " + " ++ show i2 ++
                      " modulo " ++ show edo ++ "?" )
          _ <- getChar
          putStrLn $ show $ mod (i1 + i2) edo
          go rest
  in go $ random_sequence (0,1)

-- | An example of an interactive session using this
-- can be found at /README/using-the-arithmetic-quizzer.md
quiz_sums :: Int -> IO ()
quiz_sums edo = let
  bound = edo-1
  nums = random_sequence ( bound * (-1) :: Int,
                           bound )
  go :: [Int] -> IO ()
  go nums = let
    ([n1,n2], rest) = splitAt 2 nums
    in do putStrLn (  "What is " ++ show n1 ++
                      " + " ++ show n2 ++
                      " modulo " ++ show edo ++ "?" )
          _ <- getChar
          putStrLn $ show $ mod (n1 + n2) edo
          go rest
  in go nums

random_monome_position :: [Float] -> (Int,Int,[Float])
random_monome_position nums = let
  ([col,row], rest) = splitAt 2 nums
  in ( unit_float_to_int 4 col,
       unit_float_to_int 8 row - 4,
       rest )

-- | Returns an integer in [0,max].
unit_float_to_int :: Int -> Float -> Int
unit_float_to_int max f =
  floor $ f * fromIntegral (max + 1)

-- | Hard-coded for 58-edo.
monome_position_to_edo_value :: (Int, Int) -> Int
monome_position_to_edo_value (col,row) =
  mod (col * 15 + row * 2) 58

likelyGridInterval :: Float     -> Float ->
  Int         -> Int      -> (Int,Int)    -> (Int,Int) -> Int
likelyGridInterval    colRandom rowRandom
  colInterval rowInterval (minCol,maxCol) (minRow,maxRow)
  = let
  col = floor (colRandom * fromIntegral (1 + maxCol - minCol)) + minCol
  row = floor (rowRandom * fromIntegral (1 + maxRow - minRow)) + minRow
  in (col * colInterval + row * rowInterval)


-- | * Randomness

-- | Demo: A random generation of a stream of floats.
random_stream_of_floats :: IO ()
random_stream_of_floats =
  go $ random_sequence (0 :: Float, 1) where
  go :: [Float] -> IO ()
  go nums = let
    ([randomNumber], rest) = splitAt 1 nums
    in do putStrLn "Ready?"
          _ <- getChar
          putStrLn $ show randomNumber
          go rest

random_sequence :: UniformRange a
                => (a, a) -> [a]
random_sequence (min, max) =
  unfoldr (Just . uniformR (min, max)) myStdGen

myStdGen :: R.StdGen
myStdGen = mkStdGen random_seed

random_seed :: Int
random_seed = 7 -- | PITFALL: Might want to change.
