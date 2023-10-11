module Day04Spec where
import TestUtils (doTestHspec)
import Day04 (part1, part2)
import Test.Hspec (Spec)

spec :: Spec
spec = doTestHspec 4 id [part1, part2]
