-module(day_05_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(1, day_05:part1({string, ""})).

part1_test() ->
    ?assertEqual(1, day_05:part1({file, "inputs/day_05.txt"})).

part2_example_test() ->
    ?assertEqual(1, day_05:part2({string, ""})).

part2_test() ->
    ?assertEqual(1, day_05:part2({file, "inputs/day_05.txt"})).
