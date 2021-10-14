{-# LANGUAGE
ScopedTypeVariables,
TypeApplications #-}

module Quiz_58 where

import Data.List as L
import System.Random as R
import System.Random.Internal
import Data.Fixed


g = mkStdGen 213534

random_sequence :: UniformRange a
                => (a, a) -> [a]
random_sequence (min, max) =
  unfoldr (Just . uniformR (min, max)) g

-- | Returns an integer in [0,max].
unit_float_to_int :: Int -> Float -> Int
unit_float_to_int max f =
  floor $ f * fromIntegral (max + 1)

random_monome_position :: [Float] -> (Int,Int,[Float])
random_monome_position nums = let
  ([col,row], rest) = splitAt 2 nums
  in ( unit_float_to_int 4 col,
       unit_float_to_int 8 row - 4,
       rest )

monome_position_to_edo_value :: (Int, Int) -> Int
monome_position_to_edo_value (col,row) =
  col * 15 + row * 2

spit_notes :: IO ()
spit_notes =
  go $ random_sequence (0 :: Float, 1) where
  go nums = let
    (col,row,rest) = random_monome_position nums
    in do putStrLn $ "What note is at col "
            ++ show col ++ ", row " ++ show row ++ "?"
          _ <- getChar
          putStrLn $ show $
            monome_position_to_edo_value (col,row)
          go rest

-- Just a demo.
spit_floats :: IO ()
spit_floats =
  go $ random_sequence (0 :: Float, 1) where
  go nums = let
    ([randomNumber], rest) = splitAt 1 nums
    in do putStrLn "Ready?"
          _ <- getChar
          putStrLn $ show randomNumber
          go rest
