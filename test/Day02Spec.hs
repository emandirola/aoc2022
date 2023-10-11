module Day02Spec where
import TestUtils ( doTestHspec ) 
import Day02 ( part1, part2, preRead )
import Test.Hspec (Spec)

spec :: Spec
spec = doTestHspec 2 id [part1, part2]