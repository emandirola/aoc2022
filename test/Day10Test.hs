module Main where
import TestUtils (doTest')
import Day10 (
        day10part1,
        day10part2,
        preRead
    )

main = doTest' 10 preRead [
        day10part1,
        day10part2
    ]
