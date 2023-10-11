module Day05Spec where
import TestUtils (doTestHspec)
import Day05 (part1, part2)
import Test.Hspec (Spec)

spec :: Spec
spec = doTestHspec 5 id [part1, part2]