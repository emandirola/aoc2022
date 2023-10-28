{-# LANGUAGE OverloadedStrings #-}
module Day04 (part1, part2) where
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)

parse :: BS.ByteString -> (Int, Int, Int, Int)
parse = fromRight undefined . A.parseOnly ranges
    where
        ranges = l2t <$> range `A.sepBy` ","
        range = A.decimal `A.sepBy` "-"
        l2t [[a, b], [c, d]] = (a, b, c, d)
        l2t _ = undefined

part1 :: BS.ByteString -> String
part1 input = show $ sum $ map (fromEnum . go) $ BS.lines input
    where
        go line = ((a >= c) && (b <= d)) || ((c >= a) && (d <= b))
            where
                (a, b, c, d) = parse line
part2 :: BS.ByteString -> String
part2 input = show $ sum $ map (fromEnum . go) $ BS.lines input
    where
        go line = c <= b || b >= c
            where
                (_, b, c, _) = parse line
