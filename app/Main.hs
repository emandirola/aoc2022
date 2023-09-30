module Main where

import Day01 (day01)
import Paths_aoc2022 ( getDataFileName )
import System.Environment (getArgs)

getFolder ["test"] = "test/inputs/"
getFolder _ = "src/inputs/"

main :: IO ()
main = do
  args <- getArgs
  let folder = getFolder args
  ls <- readFile =<< getDataFileName (folder ++ "day01.txt")
  day01 $ unlines $ drop 2 $ lines ls
