# Advent of Code 2025

This repository contains my solutions for the [Advent of Code 2025](https://adventofcode.com/2025) programming puzzles, written in Erlang.

## Requirements

The solutions are written in [Erlang](https://www.erlang.org/). You will need to have Erlang/OTP installed to run the solutions.

## Useful Links

-   [Erlang Official Website](https://www.erlang.org/)
-   [Erlang.mk](https://erlang.mk/)
-   [Erlang Style Guide](https://github.com/inaka/erlang_guidelines)

## Running the code

To compile the project, run:

```shell
make
```

To run a specific day's solution, you can use the Erlang shell. First, start the shell with:

```shell
make shell
```

Then, from within the shell, you can call the functions for each day. For example, to run day 1, part 1:

```erlang
day_01:part1("your_input_here").
```

## Running the tests

To run the tests for all the solutions, run:

```shell
make tests
```

## Solutions

The solutions are located in the `src/` directory, with each day's solution in a file named `day_XX.erl`. Test files are in the `test/` directory, named `day_XX_tests.erl`. Input for each day should be placed in the `inputs/` directory in a file named `day_XX.txt`.
