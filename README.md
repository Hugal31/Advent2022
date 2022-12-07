# Advent of code 2022

My take at [Advent of Code](https://adventofcode.com) 2022 while learning
Haskell.

My goal was to practice TDD and learn Haskell while doing the puzzles.

## Unit tests

You can find the tests in the [tests](tests) directory.

To run the tests, run `cabal test --test-show-details=direct`.

## Run the bench

I didn't really focused on optimizations, but I was currious to compare my
functions and solutions to those of other people. You can see the benchmarks in
the [bench](bench) folder.

To run the benchmarks, run `cabal bench -O2`, or to have a nice HTML page to read,
run `cabal run advent2022-bench -O2 -- --output out.html`
