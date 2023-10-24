module Day13Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day13 (part1, part2)

spec :: Spec
spec = doTestHspec 13 id [part1, part2]
