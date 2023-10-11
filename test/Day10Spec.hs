module Day10Spec where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day10 (
        part1,
        part2
    )

spec :: Spec
spec = doTestHspec 10 id [
        part1,
        part2
    ]
