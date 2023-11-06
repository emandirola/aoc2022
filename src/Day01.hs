module Day01 (day01) where
import Data.Ord (comparing)
import GHC.OldList (sortBy)
import GHC.Exts (Down(Down))
import Data.List.Extra (splitOn)

day01 :: String -> (Int, Int)
day01 = (,) <$> head <*> sum <$> elfs
    where
        elfs = take 3 . sortBy (comparing Down) . map (sum . map read . lines) . splitOn "\n\n"
