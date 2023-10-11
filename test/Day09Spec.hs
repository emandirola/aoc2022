module Day09Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day09 (
        part1,
        part2, preRead
    )

spec :: Spec
spec = doTestHspec 9 id [
        part1,
        part2
    ]
