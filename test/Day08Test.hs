module Main where
import TestUtils (doTest')
import Day08 (
        day08part1,
        day08part2,
        readInput
    )

main = doTest' 8 readInput [
        day08part1,
        day08part2
    ]
