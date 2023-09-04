module Changes where

import Data.List (intersperse)

import Quiz



quiz_chord_changes :: Int -> Int -> IO ()
quiz_chord_changes edo seed =
  quizKVList ("","") changes seed
  where
    changes :: [(String, String)]
    changes = [ (concat $ intersperse ", " [a,b,c],
                 "")
              | a <- ["min","maj"],
                b <- [ "bass " ++ x ++ " through chord tones"
                     | x <- ["rising", "falling"] ],
                c <- [ "root " ++ direction ++ " by " ++ show amount
                     | direction <- ["rising","falling"],
                       amount <- [1 .. edo-1] ] ]


quiz_changes3 :: Int -> IO ()
quiz_changes3 seed = quizKVList ("","") changes seed where
  changes :: [(String, String)]
  changes = [ (concat $ intersperse ", " [a,b,c,d],
               "")
            | a <- object_mod_3
            , b <- object_3
            , c <- change_3
            , d <- feel_3 ]

object_mod_3 :: [String]
object_mod_3 = ["within", "across", "merge", "split"]

object_3 :: [String]
object_3 = ["voice", "rhythm", "melody", "pitch set", "tone row"]

change_3 :: [String]
change_3 =
  [ "more", "fewer",
    "apply next change to one of",
    "vary amplitude across",
    "more", "fewer",
    "slower", "faster",
    "more duration", "fewer duration",
    "more rhythm", "less rhythm",
    "wider pitch", "narrower pitch",
    "higher pitch", "lower pitch",
    "quieter", "louder",
    "more dynamic", "less dynamic",
    "longer extrinsic", "longer intrinsic",
    "shorter extrinsic", "shorter intrinsic",
    "permute", "reverse",
    "apply next change to one of",
    "vary amplitude across",
    "symmetric", "diatonic", "xen" ]

feel_3 :: [String]
feel_3 =
  [ "funky", "grandiose", "lighthearted", "circus",
    "agitated", "peaceful", "alien", "pop", "metal", "circus",
    "wandering",
    "pedal tone",
    "more feel/intuition ~ less understood",
    "complex", "simple", "call and response",
    "symmetric chords", "diatonic chords", "xen pitch", "bichords"
  ]

quiz_changes2 :: Int -> IO ()
quiz_changes2 seed = quizKVList ("","") changes seed where
  changes :: [(String, String)]
  changes = [ (concat $ intersperse ", " [ a
                                         , c
                                           -- , "speed: " ++ show s
                                           -- , "degree: " ++ show d
                                         ],
                "")
            | (a,c) <- ac
            , s <- speeds
            , d <- degrees ]
    where
      ac :: [(String, String)]
      ac = [ (a,c)
           | (a,b) <- applicable_changes
           , c <- b ]

applicable_changes :: [ ( String,
                          [String] ) ]
applicable_changes =
  [ ( "voice",
      [ "more", "fewer", "apply next change to one of",
        "vary amplitude across" ] ),
    -- ( "rhythm",
    --   [ "more", "fewer",
    --     "longer extrinsic", "longer intrinsic",
    --     "shorter extrinsic", "shorter intrinsic",
    --     "permute", "apply next change to one of",
    --     "vary amplitude across",
    --     "odd meter"
    --     ] ),
    ( "melody",
      [ "more", "fewer",
        "slower", "faster",
        "more duration", "fewer duration",
        "more rhythm", "less rhythm",
        "wider pitch", "narrower pitch",
        "higher pitch", "lower pitch",
        "quieter", "louder",
        "more dynamic", "less dynamic",
        "longer extrinsic", "longer intrinsic",
        "shorter extrinsic", "shorter intrinsic",
        "permute", "reverse",
        "apply next change to one of",
        "vary amplitude across",
        "symmetric", "diatonic", "xen"
      ] ),
    --( "tone row",
    --  [ "more", "fewer",
    --    "wider pitch", "narrower pitch",
    --    "higher pitch", "lower pitch",
    --    "longer extrinsic", "longer intrinsic",
    --    "shorter extrinsic", "shorter intrinsic",
    --    "permute", "reverse",
    --    "apply next change to one of",
    --    "vary amplitude across",
    --    "symmetric", "diatonic", "xen"
    --  ] ),
    --( "pitch set",
    --  [ "higher", "lower",
    --    "longer extrinsic", "longer intrinsic",
    --    "shorter extrinsic", "shorter intrinsic",
    --    "permute", "reverse",
    --    "apply next change to one of",
    --    "vary amplitude across",
    --    ] ),
    ( "feel",
      [ "funky", "grandiose", "lighthearted",
        "agitated", "peaceful", "alien",
        "complex", "simple", "call and response",
        "symmetric chords", "diatonic chords", "xen pitch", "bichords"
        ] ) ]

-- | How quickly to make the change.
newtype Speed = Speed { unSpeed :: Int }
  deriving (Show, Eq, Enum, Ord, Num)

-- | From 1 to 10 should be more than enough.
speeds :: [Speed]
speeds = [1..10]

-- | How great to make the change.
newtype Degree = Degree { unDegree :: Int }
  deriving (Show, Eq, Enum, Ord, Num)

-- ^ From 1 to 10 should be more than enough.
degrees :: [Degree]
degrees = [1..10]

quiz_changes :: Int -> IO ()
quiz_changes seed = quizKVList ("","") changes seed where
  changes :: [(String, String)]
  changes = [ (concat $ intersperse ", " [a,b,c,d],
               "")
            | a <- sizes
            , b <- transformations
            , c <- parameters
            , d <- objects ]
    where
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
