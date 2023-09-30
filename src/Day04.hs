module Day04 where
import Text.Printf (printf)
import Debug.Trace (traceShowId)
import Data.List (sortBy)
import Data.Ord (comparing)

parse line = (a, b, c, d)
    where
        f = takeWhile (/=',') line
        s = drop (length f + 1) line
        [(a, b), (c, d)] = sortBy (comparing fst) $ map (read . printf "(%s)" . map repl) [f, s] :: [(Int, Int)]
        repl c
            | c == '-' = ','
            | otherwise = c

day04part1 input = show $ sum $ map (fromEnum . go) $ lines input
    where
        go line = ((a >= c) && (b <= d)) || ((c >= a) && (d <= b))
            where
                (a, b, c, d) = parse line

day04part2 input = show $ sum $ map (fromEnum . go) $ lines input
    where
        go line = c <= b || b >= c
            where
                (a, b, c, d) = parse line