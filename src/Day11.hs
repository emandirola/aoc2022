{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day11 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Char (isAlphaNum)
import GHC.Base ((<|>))
import Data.List (foldl', unfoldr)
import GHC.OldList (sortBy)
import Data.Ord (comparing)
import GHC.Exts (Down(Down))

data Monkey = Monkey {
    index :: Int,
    items :: [Int],
    operation :: Int -> Int,
    divisor :: Int,
    true :: Int,
    false :: Int,
    inspected :: Int
 }

instance Show Monkey where
    show (Monkey index items _ divisor true false inspected) = printf "%d %s %d %d %d %d" index (show items) divisor true false inspected

step :: Int -> [Monkey] -> [Monkey]
step unworry ms = foldl' oohooh ms ms
    where
        divisors = product $ map divisor ms
        lms = length ms
        oohooh :: [Monkey] -> Monkey -> [Monkey]
        oohooh !ms' Monkey{index} = monkeyDo index ms' $ monkeySee $ ms' !! index
        monkeySee :: Monkey -> [[Int]]
        monkeySee Monkey{items, operation, divisor, true, false} =
            foldl' (\ !toAdd (item :: Int) ->
                let
                    item' = operation item `div` unworry
                    dest = if item' `mod` divisor == 0 then true else false
                    res = item' `mod` divisors
                in zipWith (\ n is -> (if n == dest then is ++ [res] else is)) [0..] toAdd
            ) (replicate lms []) items
        monkeyDo :: Int -> [Monkey] -> [[Int]] -> [Monkey]
        monkeyDo i = zipWith (\ m@Monkey{index, items, inspected} is ->
            if index == i
                then m { items = is, inspected = inspected + length items }
                else m { items = items ++ is })

parse :: BS.ByteString -> [Monkey]
parse = concat . A.parseOnly (A.many1 monkey)
    where
        monkey = Monkey <$> parseDigit parseInt <*> parseList <*> parseOperation <*> parseDigit parseInt <*> parseDigit parseInt <*> parseDigit parseInt <*> pure 0
        parseList = A.skipWhile (not . A.isDigit) *> parseInt `fmap` A.takeWhile A.isDigit `A.sepBy` ", " <* A.skipWhile (/='\n') <* A.endOfLine
        parseInt = fst . fromJust . BS.readInt
        parseDigit parser = A.skipWhile (not . A.isDigit) *> parser `fmap` A.takeWhile1 A.isDigit <* A.skipWhile (/='\n') <* (A.endOfLine <|> A.endOfInput)
        parseOperation = do
            A.skipSpace *> "Operation: new = " *> A.skipSpace
            token1 <- A.takeWhile isAlphaNum <* A.skipSpace
            let t1 = if BS.all A.isDigit token1 then const (parseInt token1) else id
            operator <- A.take 1 <* A.skipSpace
            let op = if operator == "*" then (*) else (+)
            token2 <- A.takeWhile isAlphaNum <* A.skipSpace
            let t2 = if BS.all A.isDigit token2 then const (parseInt token2) else id
            return (\old -> t1 old `op` t2 old)

solve :: Int -> Int -> BS.ByteString -> String
solve unworry n bs = show $ product $ take 2 $ sortBy (comparing Down) (map inspected $ steps !! (n - 1))
    where
        monkeys = parse bs
        steps = unfoldr (\ ms -> Just (step unworry ms, step unworry ms)) monkeys

part1 :: BS.ByteString -> String
part1 = solve 3 20

part2 :: BS.ByteString -> String
part2 = solve 1 10000