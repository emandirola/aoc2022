{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TestUtils (parseInput, readInput, doTestHspec) where
import Text.Printf (printf)
import Utils
import Test.Hspec ( Spec, runIO, it, shouldBe )
import Data.List.Extra (splitOn)
import Control.Monad (when)

doTestHspec :: ToList a => Int -> (String -> a) -> Spec
doTestHspec day test = do
  (expecteds, input) <- runIO $ readInput day
  let (part1 : part2) = toList $ test input
  it "Part 1" $ do
    show part1 `shouldBe` show (head expecteds)
  when (length expecteds > 1) $ it "part 2" $ do
      show (head part2) `shouldBe` show (expecteds !! 1)

readInput :: Int -> IO ([String], String)
readInput day = parseInput <$> readFile (printf "test/inputs/day%02d.txt" day)

parseInput :: String -> ([String], String)
parseInput = ((,) <$> init <*> last) . splitOn "\n-----\n"