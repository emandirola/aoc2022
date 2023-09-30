module Main where
import TestUtils ( doTest, doTest' ) 
import Day02 ( day02part1, day02part2, preRead )

main :: IO ()
main = doTest' 2 preRead [day02part1, day02part2]