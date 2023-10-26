{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day14 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Either (fromRight)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.Extra (chunksOf)

data Path = Path {
    minX :: Int,
    maxX :: Int,
    maxY :: Int,
    path :: HashSet (Int, Int) 
}

instance Show Path where
    show = printPath

parsePath :: BS.ByteString -> Path
parsePath = fromRight undefined . A.parseOnly path
    where
        path = toPath <$> toPair `fmap` line `A.sepBy1` " -> "
        line = A.decimal `A.sepBy1` ","
        vec xs = HS.unions $ zipWith fill xs (tail xs)
        fill (x1, y1) (x2, y2) = HS.fromList [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
            where
                (minX, maxX) = (min x1 x2, max x1 x2)
                (minY, maxY) = (min y1 y2, max y1 y2)
        toPath xs =
            let
                min' = minimum $ map fst xs
                max' = maximum $ map fst xs
                height = maximum $ map snd xs
            in
                Path min' max' height $ vec xs
        toPair [x, y] = (x, y)
        toPair _ = undefined

step :: Path -> Either (HashSet (Int, Int)) (HashSet (Int, Int))
step Path{maxY, path} = go path (500, 0)
    where
        go path' (x, y)
            | y > maxY = Left path'
            | not $ down `HS.member` path' = go path' down
            | not $ left `HS.member` path' = go path' left
            | not $ right `HS.member` path' = go path' right
            | (x, y) == (500, 0) = Left path'
            | otherwise = Right $ (x, y) `HS.insert` path'
            where
                down = (x, y+1)
                left = (x-1, y+1)
                right = (x+1, y+1)

printPath :: Path -> String
printPath Path{minX, maxX, maxY, path} = unlines $ chunksOf (maxX - minX + 1) chars
    where
        chars = [chary (x, y) | y <- [0..maxY], x <- [minX..maxX]]
        chary (x, y)
            | (x, y) `HS.member` path = 'X'
            | otherwise = '.'

parsePath' :: BS.ByteString -> Path
parsePath' bs =
    let 
        paths = map parsePath $ BS.lines bs
        minX' = minimum $ map minX paths
        maxX' = maximum $ map maxX paths
        maxY' = maximum $ map maxY paths
        terrain = Path minX' maxX' maxY' $ HS.unions $ map path paths
    in terrain

part1 :: BS.ByteString -> String
part1 bs =
    let
        terrain = parsePath' bs
        go (Left _) n = n - 1 :: Int
        go (Right path) n = go ((\p -> path{path=p}) `fmap` step path) (n+1)
    in show $ go (Right terrain) 0

part2 :: BS.ByteString -> String
part2 bs =
    let
        terrain = parsePath' bs
        maxY' = maxY terrain + 2
        path' = path terrain `HS.union` HS.fromList [(x, maxY') | x <- [500-maxY'..500+maxY']]
        terrain' = terrain{maxY = maxY', minX=500-maxY', maxX=500+maxY', path=path'}
        go (Left _) n = n :: Int
        go (Right path) n = go ((\p -> path{path=p}) `fmap` step path) (n+1)
    in show $ go (Right terrain') 0
