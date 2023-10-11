module Day02 where
import Text.Printf (printf)
import Debug.Trace
import Data.List (findIndex, elemIndex)
import Data.Maybe (fromJust)

points them us =
    let
        them' = fromEnum them - fromEnum 'A'
        us' = fromEnum us - fromEnum 'X'
        p
          | them' == us' = 3
          | (them' + 1) `mod` 3 == us' = 6
          | otherwise = 0
    in p + us' + 1

play (them, us) = points them us
wins a x = Just x == a `lookup` zip ['A'..'C'] ['Y', 'Z', 'X']
points2 (them, result) =
    let
        them' = fromEnum them - fromEnum 'A'
        p
          | result == 'X' = (them' + 2) `mod` 3
          | result == 'Y' = them' + 3
          | result == 'Z' = (them' + 1) `mod` 3 + 6
    in p + 1

rotate n xs = uncurry (flip (++)) $ (n `mod` length xs) `splitAt` xs

roundPoints p@(_, x) = handPoints + resultPoints
    where
        handPoints = ((fromJust (x `elemIndex` us) - 1) `mod` 3) + 1
        resultPoints = (index `div` 3) * 3
        index = fromJust $ p `elemIndex` all
        all = zip them us
        them = cycle ['A', 'B', 'C']
        us = concatMap (`rotate` ['X', 'Y', 'Z']) [-1..1]

preRead = map (\(f:_:s) -> (f, head s)) . lines

part1 = show . sum . map play . preRead

part2 = show . sum . map points2 . preRead