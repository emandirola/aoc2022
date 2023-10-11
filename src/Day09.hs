{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiWayIf #-}
module Day09 (
        part1,
        part2,
        preRead
    ) where
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Foldable (Foldable(foldl'), traverse_)
import Data.Bifunctor (bimap)
import Data.List (nub, scanl', unfoldr)
import Debug.Trace (traceShowId, traceShow, traceId, trace)
import GHC.OldList (sort)
import Control.Monad.Fix (fix)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust)

data Move = U !Int | D !Int | L !Int | R !Int deriving Show

moves :: Move -> [(Int, Int)]
moves (U n) = replicate n ( 0,  1)
moves (D n) = replicate n ( 0, -1)
moves (L n) = replicate n (-1,  0)
moves (R n) = replicate n ( 1,  0)

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) <+> (c, d) = (a + c, b + d)
(<->) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) <-> (c, d) = (a - c, b - d)

makeMove :: ([(Int, Int)], [(Int, Int)]) -> Move -> ([(Int, Int)], [(Int, Int)])
makeMove (pos, !positions) move = foldl'
    (\(posHead:posKnots, positions) move ->
        let
            distance h t = uncurry max $ bimap abs abs (h <-> t)
            step :: (Int, Int) -> (Int, Int) -> (Int, Int)
            step (hx, hy) (tx, ty) = (tx + dx , ty + dy)
                where
                    dx = signum (hx - tx)
                    dy = signum (hy - ty)
            posKnots' = concat $ unfoldr (\(h, ks) ->
                let k = head ks
                    k' = step h k
                in if
                    | null ks -> Nothing
                    | distance h k > 1 -> Just ([k'], (k', tail ks))
                    | otherwise -> Just (ks, (h, []))
                ) (posHead', posKnots)
            posHead' = posHead <+> move
            lk = last posKnots'
            positions' =  if lk `elem` positions then positions else lk:positions
            pk = posHead':posKnots'
            res = pk `seq` positions' `seq` (pk, positions')
        in res
    )
    (pos, positions)
    (moves move)

printPos :: ([(Int, Int)], [(Int, Int)]) -> String
printPos args@(h:ks, positions) = board
    where
        ks' = zip ks ['1'..]
        board = show args ++ [p2c (x, y) | y <- reverse [0..5], x <- [0..6]]
        p2c p@(x, y)
          | p == h = 'H'
          | isJust $ p `lookup` ks' = fromJust $ p `lookup` ks'
          | p `elem` positions = '#'
          | x == 6 = '\n'
          | otherwise = '.'

preRead = map
    (\l ->
        let (dir, _:n) = break isSpace l
        in read n & case dir of
            "U" -> U
            "D" -> D
            "L" -> L
            "R" -> R
            _ -> error dir
    ) . lines

makeMoves tails = res
    where
        res = show . length . snd . foldl' makeMove initial
        --printSteps = traceId $ concatMap (printf "\n%s" . printPos) scanned
        scanned = scanl' makeMove initial
        initial = (replicate (tails + 1) (0, 0), [(0, 0)])

part1 = makeMoves 1 . preRead

part2 = makeMoves 9 . preRead
