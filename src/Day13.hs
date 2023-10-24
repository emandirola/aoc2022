{-# LANGUAGE OverloadedStrings #-}
module Day13 (part1, part2) where
import qualified Data.ByteString.Char8 as BS
import Data.List.Extra (chunksOf, elemIndices, sortBy, findIndices)
import Data.Maybe (fromMaybe)
import qualified  Data.Attoparsec.ByteString.Char8 as A
import GHC.Base (Alternative((<|>)))
import Data.Either.Extra (fromRight')

data Input = IInt Int | IList [Input] deriving (Show, Eq)

fromEither :: Either Int [Input] -> Input
fromEither (Left l) = IInt l
fromEither (Right l) = IList l

parse :: BS.ByteString -> Input
parse = fromRight' . A.parseOnly input
    where
        input = fromEither <$> A.decimal `A.eitherP` (A.char '[' *> input `A.sepBy` "," <* A.char ']')

cleaned :: BS.ByteString -> [Input]
cleaned = map parse . filter (not . BS.null) . BS.lines

check :: Input -> Input -> Bool
check i1 i2 = fromMaybe True $ check' i1 i2
    where
        check' i1' i2' = case (i1', i2') of
            (IInt p1, IInt p2)
                | p1 == p2 -> Nothing
                | otherwise -> Just $ p1 < p2
            (IList p1, IList p2) -> compareLists p1 p2
            (IInt _, IList _) -> check' (IList [i1']) i2'
            (IList _, IInt _) -> check' i1' (IList [i2'])

        compareLists [] [] = Nothing
        compareLists _ [] = Just False
        compareLists [] _ = Just True
        compareLists (x:xs) (y:ys) = check' x y <|> compareLists xs ys

part1 :: BS.ByteString -> String
part1 = show . sum . map (+1) . elemIndices True . map check' . chunksOf 2 . cleaned
    where
        check' [i1, i2] = check i1 i2
        check' _ = False

part2 :: BS.ByteString -> String
part2 = show . product . map (+1) . findIndices (`elem` divisors) . sortBy comparer . (divisors++) . cleaned
    where
        comparer a b = if check a b then LT else GT
        divisors = [parse "[[2]]", parse "[[6]]"]