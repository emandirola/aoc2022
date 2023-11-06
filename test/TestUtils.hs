{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUtils (parseInput, readInput, doTestHspec) where
import Text.Printf (printf)
import Utils
import Test.Hspec ( Spec, runIO, it, shouldBe, pendingWith )
import Data.List.Extra (splitOn)
import Data.List (isPrefixOf)
import Control.Monad

doTestHspec :: ToList a => Int -> (String -> a) -> Spec
doTestHspec day test = do
  (expecteds, input) <- runIO $ readInput day
  let [part1, part2] = Utils.toList $ test input
  it ("Part 1") $ do
    show part1 `shouldBe` show (expecteds !! 0)
  if length expecteds > 1 then
    it "part 2" $ do
      show part2 `shouldBe` show (expecteds !! 1)
  else
    return ()

readInput :: Int -> IO ([String], String)
readInput day = parseInput <$> readFile (printf "test/inputs/day%02d.txt" day)

parseInput :: String -> ([String], String)
parseInput = ((,) <$> init <*> last) . splitOn "\n-----\n"