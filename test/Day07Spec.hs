module Day07Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day07 (
    part1,
    part2
    )

spec :: Spec
spec = doTestHspec 7 id [
        part1,
        part2
    ]
