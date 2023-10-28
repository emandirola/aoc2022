module Day15Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day15 (part1, part2)

spec :: Spec
spec = doTestHspec 15 id [part1, part2]
