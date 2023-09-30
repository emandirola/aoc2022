module Day06 where
import Data.List (zip4, nub, findIndex)
import Debug.Trace (traceShowId)
import Data.Maybe (fromJust)

day06part1 = findMatching 4

day06part2 = findMatching 14

findMatching l input = go
    where
        input' = head $ lines input
        windows = map (take l . flip drop input) [0..length input' - l]
        go = show $ l + fromJust (findIndex ((==l) . length . nub) windows)