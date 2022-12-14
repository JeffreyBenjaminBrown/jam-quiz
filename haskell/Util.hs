{-# LANGUAGE RankNTypes #-}

module Util where


type Edo = Int

myPrint :: forall a f. (Show a, Foldable f) => f a -> IO ()
myPrint = mapM_ $ putStrLn . show
