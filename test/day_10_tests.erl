-module(day_10_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(1, day_10:part1({string, ""})).

part1_test() ->
    ?assertEqual(1, day_10:part1({file, "inputs/day_10.txt"})).

part2_example_test() ->
    ?assertEqual(1, day_10:part2({string, ""})).

part2_test() ->
    ?assertEqual(1, day_10:part2({file, "inputs/day_10.txt"})).
