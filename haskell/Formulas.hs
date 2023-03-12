-- TODO : given scale, degree, what's the other scale
--      : (hard) given 2 root-scale pairs, which notes in common
module Formulas where

import System.Random

import Formulas.Data
import Quiz


quizNames, quizFormulas, quizVariants, quizModes :: Int -> IO ()
quizNames =    quizKVList ("scale formula", "scale name") formulaNamePairs
quizFormulas = quizKVList ("scale name", "scale formula") nameFormulaPairs
quizVariants = quizKVList ("scale name", "set of variants") nameVariantPairs
quizModes = quizKVRingList ("scale", "mode") $ take 4 nameSetFormulaFamilies
  -- take 4 to omit the modes of the symmetric scales, which are obvious
