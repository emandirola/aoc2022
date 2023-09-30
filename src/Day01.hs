module Day01 (day01, day01part1, day01part2) where
import Paths_aoc2022 (getDataFileName)
import Data.List (sort)
import qualified Data.Ord
import Data.Ord (comparing)
import GHC.OldList (sortBy)
import GHC.Exts (Down(Down))

parse :: [String] -> [[Int]]
parse [] = []
parse calories =
    let
        (elf,ws) = break null calories
        elf' = map read elf
        ws' = drop 1 ws
    in elf':parse ws'

sums input =
    let ps = parse $ lines input
    in map sum ps

day01part1 input = show $ maximum $ sums input

day01part2 input = show $ sum $ take 3 $ sortBy (comparing Down) (sums input)

day01 input = do
    print "Day 01 part 1"
    print $ day01part1 input
    print "Day 01 part 2"
    print $ day01part2 input