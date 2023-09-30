module Main where
import Day01
import TestUtils (doTest, doTest')
 
main :: IO ()
main = doTest' 1 preRead [day01part1, day01part2]