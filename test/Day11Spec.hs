module Day11Spec (spec) where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day11 (
        part1,
        part2,
    )

spec :: Spec
spec = doTestHspec 11 id [part1, part2]
