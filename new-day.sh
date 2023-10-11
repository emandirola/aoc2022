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
module Day${day0} (part1, part2) where

part1 = undefined

part2 = undefined
EOF

cat > ./test/Day${day0}Spec.hs << EOF
module Day${day0} where
import TestUtils (doTestHspec)
import Test.Hspec (Spec)
import Day${day0} (part1, part2)

spec :: Spec
spec = doTestHspec $day id [part1, part2]
EOF
