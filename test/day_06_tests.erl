-module(day_06_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(1, day_06:part1({string, ""})).

part1_test() ->
    ?assertEqual(1, day_06:part1({file, "inputs/day_06.txt"})).

part2_example_test() ->
    ?assertEqual(1, day_06:part2({string, ""})).

part2_test() ->
    ?assertEqual(1, day_06:part2({file, "inputs/day_06.txt"})).
