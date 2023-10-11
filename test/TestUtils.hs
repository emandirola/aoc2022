{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUtils (parseInput, readInput, doTestHspec) where
import Text.Printf (printf)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import Test.Hspec ( Spec, runIO, it, shouldBe, pendingWith )
import Utils
import Debug.Trace (traceShowId)

doTestHspec :: forall a b c. (ConvertBS a, ConvertBS b) => Int -> (a -> c) -> [c -> b] -> Spec
doTestHspec day parser tests = do
  (expecteds, input) <- runIO $ readInput day parser
  mapM_ (\((part, fn), expected) ->
    it ("Part " ++ show part) $ do
      let expected' = BS.strip $ toByteString expected
      if "<expected" `BS.isPrefixOf` expected' then
        pendingWith "TODO"
      else
        BS.strip (toByteString (fn input)) `shouldBe` BS.strip (toByteString expected)
    ) $ zip (zip [1::Int ..] tests) expecteds

readInput :: (ConvertBS b) => Int -> (b -> c) -> IO ([b], c)
readInput day parser = do
  rawInput <- BS.readFile (printf "test/inputs/day%02d.txt" day)
  let (expecteds, input) = parser `fmap` parseInput rawInput
  return (expecteds, input)

parseInput :: (ConvertBS a, ConvertBS i) => i -> ([a], a)
parseInput bs = (map fromByteString e, fromByteString i)
  where
    (e, i) = Debug.Trace.traceShowId $ case go bs of
              Left str -> error str
              Right r -> r
    go = A.parseOnly strParser . toByteString
    strParser = do
      expecteds <- A.many1 expecteds'
      input <- A.takeByteString
      return (map (BS.intercalate "\n") expecteds, input)
    expecteds' = A.manyTill (A.takeWhile (/= '\n') <* A.endOfLine) (A.string "-----" *> A.endOfLine)
