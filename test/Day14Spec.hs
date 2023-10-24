module Day14Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day14 (part1, part2)

spec :: Spec
spec = doTestHspec 14 id [part1, part2]
