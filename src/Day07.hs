 {-# LANGUAGE MultiWayIf, GADTs, NamedFieldPuns, NumericUnderscores #-}
 module Day07 (
        part1,
        part2
    ) where
import Data.List (isPrefixOf, unfoldr, sortOn)
import Control.Arrow (second, Arrow (second))
import Data.Char (isSpace)
import Data.Tuple (swap)
import Text.Printf (printf)
import Data.Bifunctor (bimap)
import GHC.Base (join)

type Size = Int
type Name = String
--data Tree = Folder Name Int [Tree] | File Name Int deriving (Show)
data Node where
    Folder :: { name :: Name, size :: Size, nodes :: [Node] } -> Node
    File :: { name :: Name, size :: Size } -> Node
    deriving (Show)
--treeSize (Tree (_, s, _)) = s

--parseInput' :: Int -> [String] -> ([(Int, String, Tree)], [String])
parseInput' :: [String] -> [Node]
parseInput' input = concat $ unfoldr
    (\is ->
        let
            i:is' = is
            n = words i !! 2
            --file = (\[a, b] -> File b (read a)) . words
            file = uncurry File . second read . swap . break isSpace
            -- v1
            (consumed', rest') = join bimap (map snd) $ span ((>=0) . fst) $ unfoldr
                (\(depth, is) ->
                    let depth' = depth + nodeDepth (head is)
                        i:is' = is
                    in if null is then Nothing else Just ((depth', i), (depth', if null is' then [] else is'))
                )
                (0, is')
            -- v2
            --(consumed, rest) = join bimap (map fst) $ span ((>=0) . snd) $ zip is' $ scanl (+) 0 (map nodeDepth is')
            -- v3
            --(consumed, rest) = flip splitAt is' $ fromMaybe (length is') $ elemIndex (-1) (scanl (+) 0 (map nodeDepth is'))
            -- v4
            --(consumed, rest) = fromMaybe (is', []) $ (Just . (`splitAt` is')) =<< elemIndex (-1) (scanl (+) 0 (map nodeDepth is'))
            -- v5
            (consumed, rest) = fst (foldl accDepth (0, 0) is') `splitAt` is'
                where
                    accDepth (step, depth) input = if depth < 0 then (step, depth) else (step+1, depth + nodeDepth input)
            folder = parseInput' consumed
            s = sum $ map size folder
        in if
            | null is -> Nothing
            | "$ cd .." == i -> Just ([], is')
            | "$ cd" `isPrefixOf` i -> Just ([Folder n s folder], rest)
            | "$ ls" == i -> Just ([], is')
            | "dir" `isPrefixOf` i -> Just ([], is')
            | otherwise -> Just ([file i], is')
    )
    input

nodeDepth :: String -> Int
nodeDepth i
    | "$ cd .." == i = -1
    | "$ cd" `isPrefixOf` i = 1
    | otherwise = 0

chooseTrees :: (Node -> Bool) -> Node -> [Node]
chooseTrees p = go
    where
        go f@(Folder {nodes}) = [f | p f] ++ concatMap go nodes
        go _ = []

res = head . parseInput' . lines

part1 input = show $ sum $ map size $ chooseTrees ((< 100_000) . size) res'
    where
        res' = res input

part2 input = show $ size chosen
    where
        res' = res input
        used = size res'
        unused = 70_000_000 - used
        needed = 30_000_000 - unused
        candidates = chooseTrees ((>needed) . size) res'
        chosen = head $ sortOn size candidates

printTree :: Node -> String
printTree = printTree' 0
    where
        pre pad n = printf (replicate pad '-' ++ "%s: %d\n") (name n) (size n)
        printTree' pad n@(Folder name size tree) = pre pad n ++ concatMap (printTree' (pad+2)) tree
        printTree' pad n@(File name size) = pre pad n