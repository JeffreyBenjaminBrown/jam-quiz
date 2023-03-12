{-# LANGUAGE RankNTypes #-}

module Util where


type Edo = Int

{- | Select from a list using the decimal part of a float.
For any integer n, n retrieves the first elt,
n+0.99999 retrieves the last elt, etc.
-}
infixl 9 !!!
(!!!) :: [a] -> Float -> (Int,a)
(!!!) l f = (n, l !! n) where
  f' = f - fromIntegral (floor f) -- ensures f is in (0,1)
  n = floor $ f' * fromIntegral (length l)

myPrint :: forall a f. (Show a, Foldable f) => f a -> IO ()
myPrint = mapM_ $ putStrLn . show
