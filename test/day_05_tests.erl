-module(day_05_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(3, day_05:part1({string, "3-5
    10-14
    16-20
    12-18

    1
    5
    8
    11
    17
    32"})).

part1_test() ->
    ?assertEqual(563, day_05:part1({file, "inputs/day_05.txt"})).

part2_example_test() ->
    ?assertEqual(14, day_05:part2({string, "3-5
    10-14
    16-20
    12-18

    1
    5
    8
    11
    17
    32"})).

part2_test() ->
    ?assertEqual(338693411431456, day_05:part2({file, "inputs/day_05.txt"})).
