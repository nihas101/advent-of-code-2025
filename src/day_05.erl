-module(day_05).
-export([part1/1, part2/1]).

parseRange(Range) ->
    [A, B] = string:split(Range, "-", all),
    {range, list_to_integer(A), list_to_integer(B)}.

splitLines(Lines) ->
    SplitLines = string:split(Lines, "\n", all),
    TrimmedLines = [string:trim(X) || X <- SplitLines],
    lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines).

parseRanges(Ranges) ->
    [parseRange(X) || X <- splitLines(Ranges)].

parseIds(Ids) ->
    [list_to_integer(X) || X <- splitLines(Ids)].

parse(Input) ->
    [FreshIdRanges, Ids] = string:split(Input, "\n\n", all),
    {database, parseRanges(FreshIdRanges), parseIds(Ids)}.

% This is not perfect, but good enough I guess.
% Basing the iterations on the initial length means
% we can 'overshoot' and do unnecessary iterations
join_ranges([H|T] = FreshIdRanges) -> join_ranges(FreshIdRanges, join(H, T, []), length(FreshIdRanges)).
join_ranges(FreshIdRanges, [H|T] = JoinedRanges, 0) when length(FreshIdRanges) /= length(JoinedRanges) ->
    join_ranges(JoinedRanges, join(H, T, []), length(JoinedRanges));
join_ranges(FreshIdRanges, JoinedRanges, 0) when length(FreshIdRanges) == length(JoinedRanges) ->
    FreshIdRanges;
join_ranges(_, [H|T] = JoinedRanges, X) ->
    join_ranges(JoinedRanges, join(H, T, []), X-1).

join(P, [], Output) -> Output ++ [P];
join({range, A, B}, [{range, X, Y}|Ranges], Output) when A =< X, X =< B, A =< Y, Y =< B ->
    join({range, A, B}, Ranges, Output);
join({range, A, B}, [{range, X, Y}|Ranges], Output) when A =< X, X =< B, B =< Y ->
    join({range, A, Y}, Ranges, Output);
join({range, A, B}, [{range, X, Y}|Ranges], Output) when X =< A, A =< Y, Y =< B ->
    join({range, X, B}, Ranges, Output);
join(R1, [R2|Ranges], Output) ->
    join(R1, Ranges, Output ++ [R2]).

fresh([], _) -> false;
fresh([{range, X, Y}|_], Id) when X =< Id, Id =< Y -> true;
fresh([_|Ranges], Id) -> fresh(Ranges, Id).


part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    {database, FreshIdRanges, Ids} = parse(Input),
    JoinedRanges = join_ranges(FreshIdRanges),
    Fresh = lists:filter(fun(X) -> fresh(JoinedRanges, X) end, Ids),
    length(Fresh).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    {database, FreshIdRanges, _} = parse(Input),
    JoinedRanges = join_ranges(FreshIdRanges),
    lists:sum([(Y - X) + 1 ||{range, X, Y} <- JoinedRanges]).
