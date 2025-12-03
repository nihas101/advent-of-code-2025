-module(day_03_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(357, day_03:part1({string, "987654321111111
    811111111111119
    234234234234278
    818181911112111"})).

part1_test() ->
    ?assertEqual(17403, day_03:part1({file, "inputs/day_03.txt"})).

part2_example_test() ->
    ?assertEqual(3121910778619, day_03:part2({string, "987654321111111
    811111111111119
    234234234234278
    818181911112111"})).

part2_test() ->
    ?assertEqual(173416889848394, day_03:part2({file, "inputs/day_03.txt"})).
