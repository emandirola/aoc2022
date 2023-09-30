module Day03 where
import GHC.Base (divInt)
import Data.Char (isAsciiLower)

isIn :: Char -> String -> [Char] -> Bool
isIn x xs checked
    | x `elem` checked = False
    | otherwise = x `elem` xs

score x
    | isAsciiLower x = fromEnum x - fromEnum 'a' + 1
    | otherwise = fromEnum x - fromEnum 'A' + 27

check :: String -> String -> String -> (Char, String)
check (l:ls) rs checked
    | isIn l rs checked = (l, checked ++ [l])
    | otherwise = check ls rs (checked ++ [l])

solve1 input =
    let l = length input `divInt` 2
        repeated = uncurry check (splitAt l input) []
    in score $ fst repeated

solve2 :: (String, String, String) -> Int
solve2 input =
    let (e1, e2, e3) = input
        findTripeated (e:e1) e2 e3 checked
            | isIn e e2 checked && isIn e e3 checked = (e, checked ++ [e])
            | otherwise = findTripeated e1 e2 e3 (checked ++ [e])
        found = findTripeated e1 e2 e3 []
    in score $ fst found

take3 inputs
    | null inputs = []
    | otherwise = (x, y, z) : take3 xs
        where
            (x:y:z:xs) = inputs

day03part1 input = show $ sum $ map solve1 $ lines input
day03part2 input = show $ sum $ map solve2 $ take3 $ lines input