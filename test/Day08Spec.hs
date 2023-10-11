module Day08Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day08 (
        part1,
        part2,
        readInput
    )

spec :: Spec
spec = doTestHspec 8 id [
        part1,
        part2
    ]
