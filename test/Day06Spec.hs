module Day06Spec where
import TestUtils (doTestHspec)
import Day06 (part1, part2)
import Test.Hspec (Spec)

spec :: Spec
spec = doTestHspec 6 id [
    part1,
    part2
    ]