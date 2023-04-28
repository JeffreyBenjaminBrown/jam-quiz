module Changes where

import Data.List as L

import Quiz


-- TODO : Can I incorporate total complexity into this?

sizes, transformations, objects, parameters :: [String]

sizes = [ "greatly", "slightly" ]

transformations =
  [ "quickly"
  , "slowly"
  , "reverse" ] -- in this case, ignore parameter and size

parameters = let
  base = [ "number"
         , "speed"
         , "pitch"
         , "duration"
         , "amplitude" ]
  stat = [ "mean", "variance" ]
  in [ a ++ " " ++ b
     | a <- stat, b <- base ]

objects = [ "note"
          , "root"
          , "pitch set"
          , "rhythm"
          , "melody"
          , "tone row"
          , "voice" ]

quiz_gestalt :: Int -> IO ()
quiz_gestalt seed = quizKVList ("","") changes seed where
  changes :: [(String, String)]
  changes = [ (concat $ L.intersperse ", " [a,b,c,d],
               "")
            | a <- sizes
            , b <- transformations
            , c <- parameters
            , d <- objects ]
