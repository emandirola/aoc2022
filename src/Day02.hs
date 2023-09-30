module Day02 where
import Text.Printf (printf)
import Debug.Trace

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

points2 (them, result) =
    let
        them' = fromEnum them - fromEnum 'A'
        p
          | result == 'X' = (them' + 2) `mod` 3
          | result == 'Y' = them' + 3
          | result == 'Z' = (them' + 1) `mod` 3 + 6
    in p + 1

day02part1 input =
    let parsed = map (\(f:_:s) -> (f, head s)) $ lines input
    in show $ sum $ map play parsed

day02part2 input =
    let parsed = map (\(f:_:s) -> (f, head s)) $ lines input
    in show $ sum $ map points2 parsed