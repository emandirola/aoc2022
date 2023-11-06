{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main (main) where

import System.Environment (getArgs)
import Text.Printf (printf)
import System.TimeIt (timeItNamed)
import GHC.IO (evaluate)
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import System.CPUTime (getCPUTime)
import Control.Monad (join)
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Bifunctor (first)

data Day = forall a. (Show a, NFData a) => Day (String -> a)
days :: [Day]
days = [
      Day day01
    --, Day day02
    {-
    , [Part Day03.part1, Part Day03.part2]
    , [Part Day04.part1, Part Day04.part2]
    , [Part Day05.part1, Part Day05.part2]
    , [Part Day06.part1, Part Day06.part2]
    , [Part Day07.part1, Part Day07.part2]
    , [Part Day08.part1, Part Day08.part2]
    , [Part Day09.part1, Part Day09.part2]
    , [Part Day10.part1, Part Day10.part2]
    , [Part Day11.part1, Part Day11.part2]
    , [Part Day12.part1, Part Day12.part2]
    , [Part Day13.part1, Part Day13.part2]
    , [Part Day14.part1, Part Day14.part2]
    , [Part Day15.part1, Part Day15.part2]
  -}
  ]

main :: IO ()
main = do
  args <- getArgs
  let days' = if null args then [1..length days] else [read $ head args]
  mapM_ (uncurry runDay) (filter ((`elem` days') . snd) (zip days [1..]))
  --sequence_ ([callPart part d p | index <- indexes, let d = index `div` 2 + 1, let p = index `mod` 2 + 1, let part = parts !! index, p `elem` parts'])
  where
    times :: [(Int, String)]
    times = [(0, "us"), (3, "ms"), (6, "s")]
    toTime :: Double -> String
    toTime d = head $ map (uncurry (printf "%0.3f %s")) $ filter ((>0) . fst) $ map (first $ (d/) . (10^)) times
    runDay (Day f) n = do
      start <- getCPUTime
      res <- join $ evaluate $ f <$> Main.readInput n
      let res' = rnf res `deepseq` res
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^(6 :: Int)) :: Double
      printf "Day %d: %s\n" n $ toTime diff
      print res'

readInput :: Int -> IO String
readInput day = readFile (printf "src/inputs/day%02d.txt" day)
