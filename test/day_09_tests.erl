-module(day_09_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(50, day_09:part1({string, "7,1
    11,1
    11,7
    9,7
    9,5
    2,5
    2,3
    7,3"})).

part1_test() ->
    ?assertEqual(4760959496, day_09:part1({file, "inputs/day_09.txt"})).

part2_example_test() ->
    ?assertEqual(24, day_09:part2({string, "7,1
    11,1
    11,7
    9,7
    9,5
    2,5
    2,3
     7,3"})).

part2_test() ->
    ?assertEqual(1343576598, day_09:part2({file, "inputs/day_09.txt"})).
