{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day12 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust, fromMaybe)
import Data.List.Extra (chunksOf)
import Data.Tuple.Extra (dupe)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Hashable (Hashable(hashWithSalt))

data PlayMap = PlayMap { cells :: Vector Cell, width :: Int } deriving (Show)
data Cell = Cell { x:: Int, y :: Int, height :: Int } deriving (Show)
instance Eq Cell where
    Cell{x=x1, y=y1} == Cell{x=x2, y=y2} = x1 == x2 && y1 == y2

instance Hashable Cell where
    hashWithSalt _ Cell{x, y} = x `hashWithSalt` y

findPath :: PlayMap -> Int -> (Cell -> Bool) -> (Cell -> Int) -> (Int, Vector Cell)
findPath nodes s isEnd heuristic =
    let
        start = cells nodes V.! s
        step pq seen costs parents
            | null pq           = Nothing
            | isEnd node        = Just $ buildPath cost node parents
            | node `elem` seen  = step pq' seen costs parents
            | otherwise         = step pq'' seen' costs' parents'
            where
                ((_, (node, cost)), pq') = PQ.deleteFindMin pq
                seen' = node `HS.insert` seen
                neighbours'
                    = filter (\(c, cost') -> not (c `HS.member` seen') &&
                             (not (c `HM.member` costs) || cost' < (costs HM.! c)))
                    . map (\c -> (c, cost + 1))
                    $ neighbours nodes node
                pq'' = foldl' (\ q (c, cost') -> PQ.insert (cost' + heuristic c) (c, cost') q) pq' neighbours'
                costs' = foldl' (\ m (c, cost') -> HM.insert c cost' m) costs neighbours'
                parents' = foldl' (\ m (c, _) -> HM.insert c node m) parents neighbours'
        buildPath cost node parents = (cost, V.unfoldr (fmap (uncurry (,) . dupe) . (`HM.lookup` parents)) node V.++ V.singleton node)
    in fromJust $ step (PQ.singleton (heuristic start) (start, 0)) HS.empty HM.empty HM.empty

distance :: Cell -> Cell -> Int
distance Cell{x=x1, y=y1} Cell{x=x2, y=y2} = abs (x1 - x2) + abs (y1 - y2)

neighbours :: PlayMap -> Cell -> [Cell]
neighbours PlayMap{cells, width} c@Cell{x, y, height=height'} = map (cells V.!) . filter canGo $ [x' + width * y' | x' <- [x-1..x+2], y' <- [y-1..y+2]]
    where
        mapHeight = length cells `div` width
        inBounds n = x' >= 0 && x' < width && y' >= 0 && y' < mapHeight
            where
                x' = n `mod` width
                y' = n `div` width
        canGo n = inBounds n && distance c c' == 1 && height' - height c' <= 1
            where c' = cells V.! n

parse :: Int -> BS.ByteString -> PlayMap
parse width = flip PlayMap width . V.fromList . zipWith go [0..] . BS.unpack . BS.filter (/= '\n')
    where
        char 'S' = 'a'
        char 'E' = 'z'
        char c = c
        go i c = Cell x' y' (fromEnum (char c) - fromEnum 'a')
            where
                x' = i `mod` width
                y' = i `div` width

printPath :: PlayMap -> Vector Cell -> String
printPath input result = ('\n':) . unlines . map concat . chunksOf (width input) . map pretty . V.toList . cells $ input
    where
        --cells' = zipWith (\ c c' -> (c, chary c c')) result (tail result)
        cells' = HM.fromList $ V.toList $ V.zipWith (\c c' -> (c, chary c c')) result (V.tail result)
        red str = "\ESC[31m" ++ str ++ "\ESC[0m"
        chary c c'
            | x c < x c' = ">"
            | x c > x c' = "<"
            | y c < y c' = "V"
            | y c > y c' = "^"
            | otherwise = "E"
        pretty c
            | c `elem` result = red $ fromMaybe "S" $ cells' HM.!? c
            | otherwise = [toEnum (fromEnum 'a' + height c)]

solve :: BS.ByteString -> (Cell -> Bool) -> (Cell -> Int) -> (PlayMap, Vector Cell)
solve bs isEnd heuristic = (map', snd $ findPath map' start isEnd heuristic)
    where
        cleaned = BS.filter (/= '\n') bs
        width = fromJust $ BS.elemIndex '\n' bs
        start = fromJust $ BS.elemIndex 'E' cleaned
        map' = parse width cleaned

part1 :: BS.ByteString -> String
part1 bs = show (length (snd result) - 1)
    where
        result = solve bs (\Cell{x=x', y=y'} -> x == x' && y == y') (\Cell{x=x', y=y'} -> abs (x - x') + abs (y + y'))
        cleaned = BS.filter (/= '\n') bs
        width = fromJust $ BS.elemIndex '\n' bs
        end = fromJust $ BS.elemIndex 'S' cleaned
        x = end `mod` width
        y = end `div` width

part2 :: BS.ByteString -> String
part2 bs = show (length (snd result) - 1)
    where result = solve bs (\c -> x c == 0) x
