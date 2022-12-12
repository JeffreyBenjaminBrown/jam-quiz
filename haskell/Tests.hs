module Tests where

import Test.HUnit

import Modes


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ aTestSuite ]

aTestSuite :: Test
aTestSuite = TestList [
  TestLabel "test_nthDifferencesIn" test_nthDifferencesIn,
  TestLabel "test_minInCents_toMinInEdo" test_minInCents_toMinInEdo,
  TestLabel "test_maxInCents_toMaxInEdo" test_maxInCents_toMaxInEdo,
  TestLabel "test_MonoAscending" test_MonoAscending,
  TestLabel "test_wellBehavedScaleFamilies" test_wellBehavedScaleFamilies,
  TestLabel "test_scaleToNewEdo_many" test_scaleToNewEdo_many
  ]

test_scaleToNewEdo_many :: Test
test_scaleToNewEdo_many = TestCase $ do
  assertBool "The chromatic scale in 5-edo to 10-edo." $
    scaleToNewEdo_many 5 10 [0..4] == [[0,2..8]]

test_wellBehavedScaleFamilies :: Test
test_wellBehavedScaleFamilies = TestCase $ do
  assertBool "There is only one 6-tone scale in 12-edo with steps no greater than 2." $
    length (wellBehavedScaleFamilies 12 6 (50,250) (50,1150) (50,1150)) == 1
  assertBool "There are four 7-tone scales in 12-edo with thirds in (250,450)." $
    length (wellBehavedScaleFamilies 12 7 (50,350) (250,450) (50,1150)) == 4

test_nthDifferencesIn :: Test
test_nthDifferencesIn = TestCase $ do
  -- These tests use wide intervals to allow for floating-point error.
  assertBool "Dorian has only major and minor thirds." $
    nthDifferencesIn 2 (290,410) 12 [0,2,3,5,7,9,10]
  assertBool "The whole tone scale has only major thirds." $
    nthDifferencesIn 2 (390,410) 12 [0,2 .. 10]
  assertBool "The symmetric 9-tone scale in 12-edo has thirds outside of the range (290,410)." $
    not $
    nthDifferencesIn 2 (290,410) 12 [0,1,2, 4,5,6, 8,9,10]

test_minInCents_toMinInEdo :: Test
test_minInCents_toMinInEdo = TestCase $ do
  assertBool "minInCents_toMinInEdo 12 250 == 2" $
    minInCents_toMinInEdo 12 250 == 3
  assertBool "minInCents_toMinInEdo 41 400 == 13" $
    minInCents_toMinInEdo 41 400 == 14

test_maxInCents_toMaxInEdo :: Test
test_maxInCents_toMaxInEdo = TestCase $ do
  assertBool "maxInCents_toMaxInEdo 12 250 == 2" $
    maxInCents_toMaxInEdo 12 250 == 2
  assertBool "maxInCents_toMaxInEdo 41 400 == 13" $
    maxInCents_toMaxInEdo 41 400 == 13

test_MonoAscending :: Test
test_MonoAscending = TestCase $ do
  assertBool "modulo 3, if there are only 2 steps, and all steps must be of size exactly 1, then no scales satisfy" $
    monoAscendingFromZero 3 1 1 2 == []
  assertBool "more" $
    monoAscendingFromZero 3 1 2    2 ==
    monoAscendingFromZero 3 1 1000 2
  assertBool "modulo 6, if steps are between 2 and 3 and there are only 2 steps, then both steps must be of size 3." $
    monoAscendingFromZero 6 2 3 2 == [[0,3]]
  assertBool "modulo 6, if steps are between 2 and 4, and there are only 2 steps, then this." $
    monoAscendingFromZero 6 2 4 2 == [ [0,2], [0,3], [0,4] ]

  -- I wrote these tests before maxJump was an argument,
  -- so I just made that argument stupid-big to preserve them.
  assertBool "monoAscendingFromZero 2 1 1000 3 is empty" $
    monoAscendingFromZero 2 1 1000 3 == []
  assertBool "monoAscendingFromZero 3 1 100 2" $
    monoAscendingFromZero 3 1 1000 2 == [ [0,1], [0,2] ]
  assertBool "monoAscendingFromZero 5 2 100 3" $
    monoAscendingFromZero 6 2 1000 3 == [ [0,2,4] ]
