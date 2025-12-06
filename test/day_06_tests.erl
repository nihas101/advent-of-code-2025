-module(day_06_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(4277556, day_06:part1({string, "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "})).

part1_test() ->
    ?assertEqual(6171290547579, day_06:part1({file, "inputs/day_06.txt"})).

part2_example_test() ->
    ?assertEqual(3263827, day_06:part2({string, "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "})).

part2_test() ->
    ?assertEqual(8811937976367, day_06:part2({file, "inputs/day_06.txt"})).
