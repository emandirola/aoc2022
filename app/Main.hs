{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main (main) where

import System.Environment (getArgs)
import Text.Printf (printf)
import GHC.IO (evaluate)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Bifunctor (first)

data Day = forall a. (Show a, NFData a) => Day (String -> a)
days :: [Day]
days = [
      Day day01
    , Day day02
    , Day day03
  ]

main :: IO ()
main = do
  args <- getArgs
  let days' = if null args then [1..length days] else [read $ head args]
  mapM_ (uncurry runDay) (filter ((`elem` days') . snd) (zip days [1..]))
  where
    times :: [(Int, String)]
    times = [(0, "us"), (3, "ms"), (6, "s")]
    toTime :: Double -> String
    toTime d = head $ map (uncurry (printf "%0.3f %s")) $ filter ((>0) . fst) $ map (first $ (d/) . (10^)) times
    runDay (Day f) n = do
      input <- Main.readInput n
      start <- getCPUTime
      res <- evaluate $ f input
      let res' = rnf res `deepseq` res
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^(6 :: Int)) :: Double
      printf "Day %d: %s\n" n $ toTime diff
      print res'

readInput :: Int -> IO String
readInput day = readFile (printf "src/inputs/day%02d.txt" day)
