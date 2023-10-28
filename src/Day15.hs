module Day15 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Either (fromRight)
import Data.Char (isDigit)
import Data.List (sortOn)

data Pos = Pos {x :: Int, y :: Int} deriving (Show, Eq)
type Sensor = Pos
type Beacon = Pos

parse :: BS.ByteString -> (Sensor, Beacon)
parse = fromRight undefined . A.parseOnly parse'
    where
        parse' = (,) <$> pos <*> pos
        pos = Pos <$> decimal <*> decimal
        decimal = A.takeTill (\c -> isDigit c || '-' == c) *> A.signed A.decimal

distance :: Sensor -> Beacon -> Int
distance Pos{x=x1, y=y1} Pos{x=x2, y=y2} = abs (x1 - x2) + abs (y1 - y2)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (_, b) (c, _) = c <= b || b >= c

merge :: [(Int, Int)] -> [(Int, Int)]
merge (x1@(a, b):x2@(c, d):xs)
    | overlap (a, b) (c, d) = merge (o:xs)
    | otherwise = x1 : merge (x2:xs)
    where
        o = (min a c, max b d)
merge [x1] = [x1]
merge [] = []

part1 :: BS.ByteString -> String
part1 bs =
    let
        findAtY = fromRight undefined $ A.parseOnly A.decimal $ head $ BS.lines bs
        sb = map parse $ tail $ BS.lines bs
        detections = occupiedAt findAtY sb
        ranges = merge $ sortOn fst $ map snd detections
        used = sum $ map (\(x1, x2) -> x2 - x1 + 1) ranges
    in show $ used - 1

occupiedAt :: Int -> [(Sensor, Beacon)] -> [(Beacon, (Int, Int))]
occupiedAt findAtY sb = [
            (beacon, range)
            | (sensor, beacon) <- sb
            , let d = distance sensor beacon - abs (findAtY - y sensor)
            , let range = (x sensor - d, x sensor + d)
            , d > 0]

part2 :: BS.ByteString -> String
part2 bs =
    let
        findAtY = fromRight undefined $ A.parseOnly A.decimal $ head $ BS.lines bs
        sb = map parse $ tail $ BS.lines bs
        detections = map (`occupiedAt` sb) [0..2*findAtY]
        ranges = map (merge . sortOn fst . map snd) detections
        (y', founds) = head . filter ((==2) . length . snd) . zip [0..] $ ranges
        x' = snd (head founds) + 1
    in show $ x' * 4000000 + y'
