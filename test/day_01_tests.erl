-module(day_01_tests).
-include_lib("eunit/include/eunit.hrl").

part1_example_test() ->
    ?assertEqual(3, day_01:part1({string, "L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    "})).

part1_test() ->
    ?assertEqual(1089, day_01:part1({file, "inputs/day_01.txt"})).

part2_example_test() ->
    ?assertEqual(6, day_01:part2({string, "L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    "})).

part2_1_test() ->
    ?assertEqual(0, day_01:part2({string, "L0"})).

part2_2_test() ->
    ?assertEqual(1, day_01:part2({string, "L50"})).

part2_3_test() ->
    ?assertEqual(1, day_01:part2({string, "R50"})).

part2_4_test() ->
    ?assertEqual(1, day_01:part2({string, "L100"})).

part2_5_test() ->
    ?assertEqual(1, day_01:part2({string, "R100"})).

part2_6_test() ->
    ?assertEqual(2, day_01:part2({string, "L150"})).

part2_7_test() ->
    ?assertEqual(2, day_01:part2({string, "R150"})).

part2_8_test() ->
    ?assertEqual(7, day_01:part2({string, "L740"})).

part2_9_test() ->
    ?assertEqual(7, day_01:part2({string, "R730"})).

part2_10_test() ->
    ?assertEqual(8, day_01:part2({string, "L750"})).

part2_test() ->
    ?assertEqual(6530, day_01:part2({file, "inputs/day_01.txt"})).
