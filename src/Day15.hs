{-# LANGUAGE NamedFieldPuns #-}
module Day15 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Either (fromRight)
import Data.Char (isDigit)
import Data.List (sortOn, subsequences, find)
import Data.List.Extra (permutations)
import Data.Maybe (mapMaybe)

data Pos    = Pos {x :: Int, y :: Int} deriving (Show, Eq)
type Sensor = Pos
type Beacon = Pos

parse :: BS.ByteString -> (Sensor, Beacon)
parse = fromRight undefined . A.parseOnly parse'
    where
        parse'  = (,) <$> pos <*> pos
        pos     = Pos <$> decimal <*> decimal
        decimal = A.takeTill (\c -> isDigit c || '-' == c) *> A.signed A.decimal

distance :: Pos -> Pos -> Int
distance Pos{x=x1, y=y1} Pos{x=x2, y=y2} = abs (x1 - x2) + abs (y1 - y2)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (_, b) (c, _) = c <= b || b >= c

merge :: [(Int, Int)] -> [(Int, Int)]
merge (x1@(a, b):x2@(c, d):xs)
    | overlap (a, b) (c, d) = merge (o:xs)
    | otherwise             = x1 : merge (x2:xs)
    where
        o = (min a c, max b d)
merge [x1] = [x1]
merge []   = []

part1 :: BS.ByteString -> String
part1 bs =
    let
        findAtY    = fromRight undefined $ A.parseOnly A.decimal $ head $ BS.lines bs
        sb         = map parse $ tail $ BS.lines bs
        detections = occupiedAt findAtY sb
        ranges     = merge $ sortOn fst $ map snd detections
        used       = sum $ map (\(x1, x2) -> x2 - x1 + 1) ranges
    in show $ used - 1

occupiedAt :: Int -> [(Sensor, Beacon)] -> [(Beacon, (Int, Int))]
occupiedAt findAtY sb = [(beacon, range)
            | (sensor, beacon) <- sb
            , let d             = distance sensor beacon - abs (findAtY - y sensor)
            , let range         = (x sensor - d, x sensor + d)
            , d > 0]

data Line = Line { slope :: Int, yPos :: Int } deriving Show
instance Eq Line where
    Line{yPos=y1} == Line{yPos=y2} = y1 == y2

data Diamond = Diamond { nw :: Line, ne :: Line, sw :: Line, se :: Line } deriving Show

diamond :: Sensor -> Beacon -> Diamond
diamond s@(Pos x y) b = Diamond nw' ne' sw' se'
    where
        d           = distance s b + 1
        line sl dir = Line sl (y - sl*x + dir*d)
        nw'         = line (-1) (-1)
        ne'         = line   1    1
        sw'         = line   1  (-1)
        se'         = line (-1)   1

combinations :: Int -> [a] -> [[a]]
combinations k = filter ((==k) . length) . subsequences

findPoint :: [Diamond] -> Maybe Pos
findPoint ds = getPoint `fmap` find hasEmptyPoint (permutations ds)

hasEmptyPoint :: [Diamond] -> Bool
hasEmptyPoint [d1, d2, d3, d4] = nw d1 == se d3 && ne d2 == sw d4
hasEmptyPoint _                = undefined

getPoint :: [Diamond] -> Pos
getPoint (Diamond{nw}:Diamond{ne}:_) = Pos x' y'
    where
        (s1, s2) = (slope nw, slope ne)
        (y1, y2) = (yPos nw , yPos ne)
        x'       = (y2 - y1) `div` (s1 - s2)
        y'       = s1 * x' + y1
getPoint _                           = undefined

part2 :: BS.ByteString -> String
part2 bs = show $ x * 4000000 + y
    where
        sb       = map parse $ tail $ BS.lines bs
        sensors  = map fst sb
        diamonds = map (uncurry diamond) sb
        points   = mapMaybe findPoint . combinations 4 $ diamonds
        Pos x y  = head $ filter (`notElem` sensors) points