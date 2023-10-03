module Day10 (
        day10part1,
        day10part2,
        preRead
    ) where
import Data.List (foldl', scanl')
import Data.Char (isNumber)
import Debug.Trace (trace, traceShowId, traceShow)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import GHC.Base ((<|>))

preRead = lines

third (_, _, t) = t

day10part1 :: [String] -> String
day10part1 = show . third . foldl' f (0, 1, 0)
    where
        step :: Int -> Int -> Int -> Maybe Int
        step cycle x strength = if shouldCount (cycle + 1) then Just $ strength + (cycle + 1) * x else Nothing
        shouldCount cycle = (cycle - 20) `mod` 40 == 0
        f (cycle, x, strength) "noop" = (cycle + 1, x, fromMaybe strength $ step cycle x strength)
        f (cycle, x, strength) instr = (cycle + 2, x + dx, fromMaybe strength $ step (cycle+1) x strength <|> step cycle x strength)
            where dx = read $ dropWhile (not . (\x -> x == '-' || isNumber x)) instr

day10part2 _ = "<expected part2>" --this way we can test only part1
