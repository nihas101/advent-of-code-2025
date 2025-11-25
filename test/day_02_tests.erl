-module(day_02_tests).
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(1, day_02:part1(1)).

part2_test() ->
    ?assertEqual(1, day_02:part2(1)).
