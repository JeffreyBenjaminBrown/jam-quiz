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
  TestLabel "test_MonoAscending" test_MonoAscending ]

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
  assertBool "monoAscendingFromZero 2 3 is empty" $
    monoAscendingFromZero 2 1 3 == []
  assertBool "monoAscendingFromZero 3 1 2" $
    monoAscendingFromZero 3 1 2 == [ [0,1], [0,2] ]
  assertBool "monoAscendingFromZero 5 2 3" $
    monoAscendingFromZero 6 2 3 == [ [0,2,4] ]
