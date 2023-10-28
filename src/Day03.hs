module Day03 (part1, part2) where
import Data.Char (isAsciiLower)
import qualified Data.ByteString.Char8 as BS
import Data.List.Extra (chunksOf)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)

isIn :: Char -> BS.ByteString -> HashSet Char -> Bool
isIn x xs checked
    | x `elem` checked = False
    | otherwise = x `BS.elem` xs

score :: Char -> Int
score x
    | isAsciiLower x = fromEnum x - fromEnum 'a' + 1
    | otherwise = fromEnum x - fromEnum 'A' + 27

check :: BS.ByteString -> BS.ByteString -> HashSet Char -> (Char, HashSet Char)
check lss rs checked
    | isIn l rs checked = (l, HS.insert l checked)
    | otherwise = check ls rs (HS.insert l checked)
    where
        l = BS.head lss
        ls = BS.tail lss

solve1 :: BS.ByteString -> Int
solve1 input =
    let l = BS.length input `div` 2
        repeated = uncurry check (BS.splitAt l input) HS.empty
    in score $ fst repeated

solve2 :: [BS.ByteString] -> Int
solve2 [e1, e2, e3] =
    let findTripeated e1' e2' e3' checked
            | isIn e e2' checked && isIn e e3' checked = (e, HS.insert e checked)
            | otherwise = findTripeated e1'' e2' e3' (HS.insert e checked)
            where
                e = BS.head e1'
                e1'' = BS.tail e1'
        found = findTripeated e1 e2 e3 HS.empty
    in score $ fst found
solve2 _ = undefined

part1 :: BS.ByteString -> String
part1 input = show $ sum $ map solve1 $ BS.lines input

part2 :: BS.ByteString -> String
part2 input = show $ sum $ map solve2 $ chunksOf 3 $ BS.lines input