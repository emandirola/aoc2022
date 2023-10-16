{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import Text.Printf (printf)
import Utils
import qualified Data.ByteString.Char8 as BS
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

data Part = forall a b. (ConvertBS a, ConvertBS b) => Part (a -> b)
parts :: [Part]
parts = [
      Part Day01.part1, Part Day01.part2
    , Part Day02.part1, Part Day02.part2
    , Part Day03.part1, Part Day03.part2
    , Part Day04.part1, Part Day04.part2
    , Part Day05.part1, Part Day05.part2
    , Part Day06.part1, Part Day06.part2
    , Part Day07.part1, Part Day07.part2
    , Part Day08.part1, Part Day08.part2
    , Part Day09.part1, Part Day09.part2
    , Part Day10.part1, Part Day10.part2
    , Part Day11.part1, Part Day11.part2
    , Part Day12.part1, Part Day12.part2
  ]

main :: IO ()
main = do
  args <- getArgs
  let days = if null args then [1..length parts `div` 2] else [read $ head args]
  let parts' = if length args < 2 then [1, 2] else [read (args !! 1)]
  let indexes = concat [[(day - 1) * 2, (day - 1) * 2 + 1] | day <- days]
  sequence_ ([callPart part d p | index <- indexes, let d = index `div` 2 + 1, let p = index `mod` 2 + 1, let part = parts !! index, p `elem` parts'])

callPart :: Part -> Int -> Int -> IO ()
callPart (Part f) day part = do
  input <- Main.readInput day
  result <- timeItNamed label $ evaluate $ f input
  let result' = toByteString result
  putStrLn $ printf "%s: %s" label . BS.unpack $ result'
  where
    label :: String
    label = printf "Day %d Part %d" day part

readInput :: (ConvertBS a) => Int -> IO a
readInput day = fromByteString `fmap` BS.readFile (printf "src/inputs/day%02d.txt" day)
