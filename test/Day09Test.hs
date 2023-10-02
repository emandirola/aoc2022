module Main where
import TestUtils (doTest, doTest')
import Day09 (
        day09part1,
        day09part2, preRead
    )

main = doTest' 9 preRead [
        day09part1,
        day09part2
    ]
