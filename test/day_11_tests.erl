-module(day_11_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(1, day_11:part1({string, ""})).

part1_test() ->
    ?assertEqual(1, day_11:part1({file, "inputs/day_11.txt"})).

part2_example_test() ->
    ?assertEqual(1, day_11:part2({string, ""})).

part2_test() ->
    ?assertEqual(1, day_11:part2({file, "inputs/day_11.txt"})).
