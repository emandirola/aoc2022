module Day01 (day01, part1, part2, preRead) where
import Data.List (unfoldr, uncons)
import Data.Ord (comparing)
import GHC.OldList (sortBy, foldl')
import GHC.Exts (Down(Down))
import Data.Maybe (isJust, fromJust, catMaybes, isNothing)
import Text.Read (readMaybe)

parse :: [Maybe Int] -> [[Int]]
parse [] = []
parse calories =
    let
        (elf,ws) = break isNothing calories
        ws' = drop 1 ws
    in catMaybes elf:parse ws'

parse2 :: [Maybe Int] -> [Int]
parse2 input = concat $ unfoldr
    (\lines -> ((\ (a, b) -> Just ([sum $ catMaybes a], snd `fmap` uncons b)) . span isJust) =<< lines)
    (Just input)

parse3 :: Int -> [Maybe Int] -> [Int]
parse3 n input = (\(m, s) -> [sum $ take n $ sortBy (comparing Down) $ s:m]) $ foldl'
    (\(maxis, sum') current ->
        let
            maxis' = take n $ sortBy (comparing Down) $ sum':maxis
        in if isNothing current then (maxis', 0) else (maxis, sum' + fromJust current)
    )
    ([], 0)
    input

sums :: [Maybe Int] -> Int -> [Int]
sums input n =
    let ps = map sum $ parse input
        ps' = parse2 input
        ps'' = parse3 n input
    in ps''

preRead :: String -> [Maybe Int]
preRead input = map readMaybe $ lines input

part1 input = show $ maximum $ sums (preRead input) 1

part2 input = show $ sum $ take 3 $ sortBy (comparing Down) (sums (preRead input) 3)

day01 input = do
    print "Day 01 part 1"
    print $ part1 input
    print "Day 01 part 2"
    print $ part2 input