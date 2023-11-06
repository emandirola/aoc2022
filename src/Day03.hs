module Day03  where
import Data.List.Extra ( chunksOf, foldl1', elemIndices )
import Data.List (intersect)

score :: [String] -> Int
score = prio . head . foldl1' intersect 
    where
        prio = (+1) . head . (`elemIndices` (['a'..'z'] ++ ['A'..'Z']))

split :: String -> [String]
split = (\(a, b) -> [a, b]) . (splitAt <$> (`div` 2) . length <*> id)

day03 :: String -> (Int, Int)
day03 = (,)
    <$> sum . map (score . split)
    <*> sum . map score . chunksOf 3
    <$> lines