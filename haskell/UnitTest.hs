module UnitTest
(
  TestCase
 ,TestCollection
 ,TestResult
 ,TestBody(..)
 ,passed
 ,failed
 ,results
 ,allSuccesses
 ,allFailures
) where

import Control.Applicative

data TestBody a = Test a a deriving (Eq,Show)

type TestCase a = (String, TestBody a)
type TestCollection a = [TestCase a]
type TestCaseResult = (String)
type TestResult = ([TestCaseResult], [TestCaseResult])

passed :: (Eq a) => TestCase a -> Bool
passed (_, (Test actual expected)) = actual == expected

failed :: (Eq a) => TestCase a -> Bool
failed = not . passed

successes :: (Eq a) => (Show a) => TestCollection a -> [TestCaseResult]
successes ts = [ show u | u <- ts, passed u]

failures :: (Eq a) => (Show a) => TestCollection a -> [TestCaseResult]
failures ts = [ show u | u <- ts, failed u]

results :: (Eq a) => (Show a) => TestCollection a -> TestResult
results ts = (successes ts, failures ts)

allSuccesses :: [TestResult] -> [String]
allSuccesses results = concat (map fst results)

allFailures :: [TestResult] -> [String]
allFailures results = concat (map snd results)