-module(day_01).
-export([part1/1, part2/1]).

-define(INITIAL_POS, 50).
-define(MOD, 100).

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    lists:map(fun([Dir|Dist]) -> {rot, Dir, list_to_integer(Dist)} end, NonEmptyLines).

next(CurrentAbs, {rot, $R, Dist}) -> CurrentAbs + Dist;
next(CurrentAbs, {rot, $L, Dist}) -> CurrentAbs - Dist.

rotate({dial, _, [], _} = R) -> R;
rotate({dial, Curr, [Rot|T], States}) ->
    Next = next(Curr, Rot),
    rotate({dial, Next, T, States ++ [{Curr, Next}]}).

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    State = {dial, ?INITIAL_POS, parse(Input), []},
    {dial, _, _, DialStates} = rotate(State),
    Positions = [?INITIAL_POS | lists:map(fun({_, Aft}) -> Aft end, DialStates)],
    Zeroes = lists:filter(fun(AbsDialPos) -> (AbsDialPos rem 100) == 0 end, Positions),
    length(Zeroes).

floor_div(A, B) when A >= 0 -> A div B;
floor_div(A, B) -> (A - B + 1) div B.

count_passing_zero(R, []) -> R;
count_passing_zero(R, [{Prev, Prev}|T]) -> count_passing_zero(R, T);
count_passing_zero(R, [{Prev, Next}|T]) ->
    Min = min(Prev, Next),
    Max = max(Prev, Next),
    % Count complete boundaries crossings
    CrossedBoundary = floor_div(Max - 1, ?MOD) - floor_div(Min, ?MOD),
    % Count if landing on a boundary, unless we started on one
    OnBoundary = if
        (Next rem ?MOD) == 0, (Prev rem ?MOD) /= 0 -> 1;
        true -> 0
    end,
    count_passing_zero(R + CrossedBoundary + OnBoundary, T).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    State = {dial, ?INITIAL_POS, parse(Input), []},
    {dial, _, _, DialStates} = rotate(State),
    count_passing_zero(0, DialStates).
