module Tests where

import Test.HUnit

import Modes


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ aTestSuite ]

aTestSuite :: Test
aTestSuite = TestList [
  TestLabel "testThirdsIn" testThirdsIn,
  TestLabel "testMonoAscending" testMonoAscending ]

testThirdsIn :: Test
testThirdsIn = TestCase $ do
  -- These tests use wide intervals to allow for floating-point error.
  assertBool "Dorian has only major and minor thirds." $
    thirdsIn (290,410) 12 [0,2,3,5,7,9,10]
  assertBool "The whole tone scale has only major thirds." $
    thirdsIn (390,410) 12 [0,2 .. 10]
  assertBool "The symmetric 9-tone scale in 12-edo has thirds outside of the range (290,410)." $
    not $
    thirdsIn (290,410) 12 [0,1,2, 4,5,6, 8,9,10]

testMonoAscending :: Test
testMonoAscending = TestCase $ do
  assertBool "monoAscendingFromZero 2 3 is empty" $
    monoAscendingFromZero 2 1 3 == []
  assertBool "monoAscendingFromZero 3 1 2" $
    monoAscendingFromZero 3 1 2 == [ [0,1], [0,2] ]
  assertBool "monoAscendingFromZero 5 2 3" $
    monoAscendingFromZero 5 2 3 == [ [0,2,4] ]
