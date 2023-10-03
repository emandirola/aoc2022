#! /usr/bin/env bash

day="$1"
if [[ "$day" == "" ]]; then
    echo "No day supplied!"
    exit 1
fi
day0=$(printf "%02d" $day)

if [[ -e "./src/inputs/day${day0}.txt" ]]; then
    echo "Day already created"
    exit 2
fi

touch ./src/inputs/day${day0}.txt

cat > ./test/inputs/day${day0}.txt << EOF
<expected part1>
-----
<expected part2>
-----
<test input>
EOF

cat > ./src/Day${day0}.hs << EOF
module Day$day0 (
        day${day0}part1,
        day${day0}part2,
        preRead
    ) where

preRead = id

day${day0}part1 = _

day${day0}part2 = "<expected part2>" --this way we can test only part1
EOF

cat > ./test/Day${day0}Test.hs << EOF
module Main where
import TestUtils (doTest')
import Day${day0} (
        day${day0}part1,
        day${day0}part2,
        preRead
    )

main = doTest' $day preRead [
        day${day0}part1,
        day${day0}part2
    ]
EOF

cat >> ./aoc2022.cabal << EOF

test-suite day${day0}-test
    import: tests-common
    type: exitcode-stdio-1.0
    main-is: Day${day0}Test.hs
EOF

sed -i "/^\s*Day${day0},\s*$/d" aoc2022.cabal
sed -i "s/.*hs-source-dirs: src.*/        Day${day0},\n&/" aoc2022.cabal