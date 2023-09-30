{-# LANGUAGE RankNTypes #-}
module TestUtils (doTest, doTest') where
import System.Exit ( exitFailure, exitSuccess, die )
import Paths_aoc2022 ( getDataFileName )
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts (failures, errors),
      Test(TestCase, TestList, TestLabel), testCaseCount, showCounts )
import Text.Printf (printf)
import System.Environment (getArgs)
import Debug.Trace (traceShow, trace)
import GHC.Base (when)
import Data.Functor (($>))

testCase input expected fn = TestCase $ assertEqual ("Should return " ++ show expected) expected $ fn input

dayTest tests = TestList $ zipWith (TestLabel . printf "Day %02d") [1::Int ..] tests

readInput file = readFile =<< getDataFileName file

getTestFile day args = printf "%s/inputs/day%02d.txt" base day
  where
    base
      | "full" `elem` args = "src"
      | otherwise = "test"

readTestFile day args parts = do
  file <- readInput $ getTestFile day args
  let expecteds = take (2 * parts) $ lines file
  let expecteds' = [expected | (i, expected) <- zip [0..] expecteds, even i]
  let input = unlines $ drop (2 * parts) $ lines file
  if "full" `elem` args then
    return ([], file)
  else
    return (expecteds', input)

actualTest expecteds input fns = do
  let testCases = zipWith (testCase input) expecteds fns
  result <- runTestTT $ dayTest testCases
  if failures result > 0 || errors result > 0 then exitFailure else exitSuccess
  return result

doTest day = doTest' day id

doTest' :: forall a. Int -> (String -> a) -> [a -> String] -> IO ()
doTest' day parser fns = do
  args <- getArgs
  (expecteds, input) <- readTestFile day args (length fns)
  res <- if "full" `elem` args then
    return (map ($ parser input) fns)
  else
    (:[]) . showCounts <$> actualTest expecteds (parser input) fns
  print res