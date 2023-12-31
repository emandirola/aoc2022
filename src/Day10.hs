module Day10 (part1, part2) where
import Data.List (foldl')
import Data.Char (isNumber)
import Data.Maybe (fromMaybe)
import GHC.Base ((<|>))
import Prelude hiding (cycle)

third :: (a, b, c) -> c
third (_, _, t) = t

part1 :: String -> String
part1 = show . third . foldl' f (0::Int, 1::Int, 0::Int) . lines
    where
        step cycle x strength = if shouldCount (cycle + 1) then Just $ strength + (cycle + 1) * x else Nothing
        shouldCount cycle = (cycle - 20) `mod` 40 == 0
        f (cycle, x, strength) "noop" = (cycle + 1, x, fromMaybe strength $ step cycle x strength)
        f (cycle, x, strength) instr = (cycle + 2, x + dx, fromMaybe strength $ step (cycle+1) x strength <|> step cycle x strength)
            where dx = read $ dropWhile (not . (\x -> x == '-' || isNumber x)) instr

part2 :: String -> String
part2 = ('\n':) . print40 . reverse . third . foldl' f (1::Int, 1::Int, "") . lines
    where
        print40 [] = ""
        print40 xs = h ++ "\n" ++ print40 t
            where
                (h, t) = splitAt 40 xs
        charAt cycle x = if cm40 == x || cm40 == x + 1 || cm40 == x + 2 then '#' else '.'
            where
                cm40 = cycle `mod` 40
        f (cycle, x, crt) "noop" = (cycle + 1, x, charAt cycle x : crt)
        f (cycle, x, crt) instr = (cycle + 2, x + dx, charAt (cycle+1) x : charAt cycle x : crt)
            where
                dx = read $ drop 5 instr
