# Advent of Code 2022

In Haskell with Stack and HUnit

https://adventofcode.com/2022

---

### New day
1) Create boilerplate for a new day with
```
# ./new-day.sh number
```
2) Paste your sample input in `./test/inputs/day<nn>.txt` and fix the expected outputs
3) Paste your personal input in `./src/inputs/day<nn>.txt`
3) Then hack away at `./src/Day<nn>.hs`

---

### Running / Testing
```
$ ./run-day.sh -d day [-p] [-f]
```
- `-d day` for day (number)
- `-f` for full input (read from `./src/inputs/day<nn>.txt`)
- `-p` for profiling