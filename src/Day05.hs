module Day05 where
import Data.Char (isSpace, isNumber)
import Debug.Trace (traceShowId, traceShow)
import Data.Foldable ( Foldable(foldr'), foldl' )
import Data.List (findIndices, unfoldr, uncons, isSuffixOf, isPrefixOf)
import Data.Tuple (swap)
import Control.Arrow (Arrow(first), second)
import Data.Maybe (fromMaybe, isJust, fromJust, listToMaybe, mapMaybe, catMaybes)
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Control.Monad (liftM2)
import Text.Read (readMaybe)

type Stacks = [String]
type Move = [Int]

day05part1 = gogogo True

day05part2 = gogogo False

gogogo :: Bool -> String -> String
gogogo reversed input = map fst $ mapMaybe uncons $ go stacks moves
    where
        ls = lines input
        stacks = buildStacks ls
        moves = map parseMove $ dropWhile (not . isPrefixOf "move") ls
        go = foldl (move reversed)

parseMove :: String -> Move
parseMove = mapMaybe readMaybe . words

move :: Bool -> Stacks -> Move -> Stacks
move reversed stacks [qty, from, to] = zipWith choose [1..] stacks
    where
        from' = stacks !! (from - 1)
        to' = stacks !! (to - 1)
        (moving, from'') = splitAt qty from'
        to'' = (if reversed then reverse else id) moving ++ to'
        choose i s
            | i == to = to''
            | i == from = from''
            | otherwise = s

buildStacks :: [String] -> Stacks
buildStacks input = buildStacks' stackInput $ replicate qty []
    where
        stackInput = takeWhile (not . isNumber . (!!1)) input
        qty = (length . head) input `div` 4 + 1
        isStack (i, _) = (i - 1) `mod` 4 == 0
        charToStack = filter (not . isSpace) . (:[]) . snd
        items input = map charToStack $ filter isStack $ zip [0..] input
        buildStacks' (input:is) stacks = buildStacks' is $ zipWith (++) stacks $ items input
        buildStacks' _ stacks = stacks