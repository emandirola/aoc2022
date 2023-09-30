module Main where

import Day09 (day09part2)
import Paths_aoc2022 ( getDataFileName )
import System.Environment (getArgs)

getFolder ["test"] = "test/inputs/"
getFolder _ = "src/inputs/"

main :: IO ()
main = do
  let a = day09part2 []
  print a