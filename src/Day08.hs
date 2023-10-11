{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Day08 (
        part1,
        part2,
        readInput
    ) where
import Data.List ( nub, sort, transpose, uncons, find, findIndices )
import GHC.OldList (foldl')
import Data.Maybe (isNothing)
import Control.Arrow (Arrow(second))
import Data.Char (ord)
import Data.Bifunctor (bimap)
import GHC.Base (join)
import GHC.Ix (Ix(inRange))

data Tree where
  Tree :: {x :: Int, y :: Int, height :: Int} -> Tree
  deriving (Show, Eq, Ord)

swapxy :: Tree -> Tree
swapxy (Tree {x, y, height}) = Tree y x height

go1 :: Int -> [Int] -> [Tree]
go1 y line = map (\(x, h) -> Tree x y h) $ sort $ nub $ fst left' ++ fst right'
    where
        line' = zip [0..] line
        m = maximum line
        left' = findAscending line'
        right' = findAscending' line'
        findAscending = foldl' fold' ([], (-1, -1)) . takeWhile ((<=m) . snd)
        findAscending' = foldr (flip fold') ([], (-1, -1)) . takeWhile ((<=m) . snd)
        fold' (!acc, p@(_,prev)) c@(i, cur) = if cur > prev then (c:acc, c) else (acc, p)

seenTrees input = (horizontal, vertical', all)
    where
      horizontal = concat $ zipWith go1 [0..] input
      vertical = zipWith ((map swapxy .) . go1) [0..] $ transpose input
      vertical' = filter (\(Tree x' y' _) -> isNothing $ find (\(Tree x'' y'' _) -> y' == y'' && x' == x'') horizontal) (concat vertical)
      all = horizontal ++ vertical'

readInput :: String -> [[Int]]
readInput input = map (map (flip (-) (ord '0') . ord)) $ lines input -- read es lento y usa 10 veces mas memoria

inputToTree :: [[Int]] -> [[Tree]]
inputToTree = zipWith (flip zipWith [0..] . Tree) [0..]

part1 input = show $ length all
  where
    (_, _, all) = seenTrees $ readInput input

part2 input = show found
  where
    input' = readInput input
    size = length input' - 1
    tree x y = trees !! y !! x
    trees = inputToTree input'
    treest = transpose trees
    isSafe t = inRange (1, size) `all` [x t, y t]
    (_, _, all') = seenTrees input'
    found = maximum [go t | t <- all', isSafe t]
    fixedSpan p xs = second (maybeTail' []) $ p `span` xs
    maybeHead' n = maybe n fst . uncons
    maybeTail' n = maybe n snd . uncons
    go t@(Tree {x=x', y=y', height=h}) = product $ map (uncurry (*)) [hori', vert']
      where
        hori = trees !! y'
        vert = treest !! x'
        cmp cur = height cur >= h
        hori' = join bimap (abs . (x'-)) $ find' (<x') hori
        vert' = join bimap (abs . (y'-)) $ find' (<y') vert
        find' fn xs = bimap (maybeHead' 0 . reverse) (maybeHead' size) $ fn `fixedSpan` findIndices cmp xs