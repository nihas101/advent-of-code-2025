-module(day_04_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(13, day_04:part1({string, "..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@."})).

part1_test() ->
    ?assertEqual(1393, day_04:part1({file, "inputs/day_04.txt"})).

part2_example_test() ->
    ?assertEqual(43, day_04:part2({string, "..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@."})).

part2_test() ->
    ?assertEqual(8643, day_04:part2({file, "inputs/day_04.txt"})).
