{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Day08 (
        part1,
        part2
    ) where
import Data.List ( nub, sort, transpose, uncons, find, findIndices )
import GHC.OldList (foldl')
import Data.Maybe (isNothing)
import Control.Arrow (Arrow(second))
import Data.Char (ord)
import Data.Bifunctor (bimap)
import GHC.Base (join)
import GHC.Ix (Ix(inRange))
import Debug.Trace (traceWith, traceShowId, traceShowWith)
import GHC.IO (unsafePerformIO)

showMatrix :: Show a => [[a]] -> String
showMatrix = unlines . map (concat . pad . map show)
    where
        pad :: [String] -> [String]
        pad xs = map pad' xs
            where
                pad' x = take maxLength (' ' : x ++ repeat ' ')
                maxLength = (+2) . maximum . map length $ xs

visibleTrees :: [[Int]] -> [[Bool]]
visibleTrees = zipWith (zipWith (||)) <$> visibles <*> transpose . visibles . transpose
    where
        !visibleFromLeft = zipWith (>) <*> scanl max (-1)
        !visibleFromRight = zipWith (>) <*> tail . scanr max (-1)
        visibles = map $ zipWith (||) <$> visibleFromLeft <*> visibleFromRight

part1 :: String -> String
part1 input = show $ length $ filter id $ concat $ visibleTrees trees
    where
        input = unsafePerformIO $ readFile "src/inputs/day08.txt"
        trees = map (map $ read . (:[])) . lines $ input

part2 :: String -> String
part2 input = undefined