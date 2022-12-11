{-# LANGUAGE RankNTypes #-}

module Util where


myPrint :: forall a f. (Show a, Foldable f) => f a -> IO ()
myPrint = mapM_ $ putStrLn . show
