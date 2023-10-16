module Day12Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day12 (part1, part2)

spec :: Spec
spec = doTestHspec 12 id [part1, part2]
