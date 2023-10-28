module Day01 (part1, part2, preRead) where
import Data.Ord (comparing)
import GHC.OldList (sortBy)
import GHC.Exts (Down(Down))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (isJust, catMaybes)

sums :: [Maybe Int] -> [Int]
sums [] = []
sums (Nothing:is) = sums is
sums is = sum (catMaybes $ takeWhile isJust is) : sums (dropWhile isJust is)

preRead :: BS.ByteString -> [Maybe Int]
preRead input = map (eitherToMaybe . A.parseOnly A.decimal) (BS.lines input)

part1 :: BS.ByteString -> String
part1 input = show $ maximum $ sums (preRead input)

part2 :: BS.ByteString -> String
part2 input = show $ sum $ take 3 $ sortBy (comparing Down) (sums (preRead input))
