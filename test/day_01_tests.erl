-module(day_01_tests).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(1, day_01:part1(1)).

part2_test() ->
    ?assertEqual(1, day_01:part2(1)).
