module Day01Spec where
import Day01
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
 
spec :: Spec
spec = doTestHspec 1 id [part1, part2]