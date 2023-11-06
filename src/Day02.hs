module Day02 where
import Data.List.Extra (elemIndices)

play :: (Int, Int) -> Int
play (them, us) = us + 1 + [0, 3, 6] !! ((them - us + 1) `mod` 3)

play2 :: (Int, Int) -> Int
play2 (them, outcome) = outcome + them + 1 `mod` 3 + 1

day02 :: String -> (Int, Int)
day02 = (,) <$> sum . map play <*> sum . map play2 <$> parse'
    where
        parse' = map ((,) <$> parse 0 ['A'..'C'] <*> parse 2 ['X'..'Z']) <$> lines
        parse n cs = head . (`elemIndices` cs) . (!!n)