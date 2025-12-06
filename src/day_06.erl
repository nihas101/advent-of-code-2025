-module(day_06).
-export([part1/1, part2/1]).

parse_number_or_op([Op|_]) when Op == $+; Op == $* -> Op;
parse_number_or_op(N) -> N.

filter_digits(Line) ->
    lists:filter(fun(X) -> X /= "" end, string:tokens(Line, " ")).

parse_line(Line) ->
    lists:map(fun parse_number_or_op/1, filter_digits(Line)).

parse(Lines) ->
    SplitLines = string:split(Lines, "\n", all),
    TrimmedLines = [string:trim(X) || X <- SplitLines],
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    lists:map(fun(X) -> parse_line(X) end, NonEmptyLines).

transpose(L) -> transpose(L, []).
transpose([H|_], Res) when length(H) == 0 -> Res;
transpose(Lists, Res) ->
    NextRes = Res ++ [lists:reverse(lists:map(fun hd/1, Lists))],
    transpose(lists:map(fun tl/1, Lists), NextRes).

split_last([H|T]) -> split_last(T, {[], H}).
split_last([], R) -> R;
split_last([H|T], {A, B}) -> split_last(T, {A ++ [B], H}).

product(List) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, List).

solve(Ops, Numbers) -> solve(Ops, Numbers, []).
solve([], [], Res) -> Res;
solve([Op|Ops], [Ns|Numbers], Res) when Op == $* ->
    Next = Res ++ [product(lists:map(fun(X) -> list_to_integer(string:trim(X)) end, Ns))],
    solve(Ops, Numbers, Next);
solve([Op|Ops], [Ns|Numbers], Res) when Op == $+ ->
    Next = Res ++ [lists:sum(lists:map(fun(X) -> list_to_integer(string:trim(X)) end, Ns))],
    solve(Ops, Numbers, Next).

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    {Numbers, Ops} = split_last(parse(Input)),
    lists:sum(solve(Ops, transpose(Numbers))).

chunk_by(L, Fn) -> chunk_by(L, Fn, []).
chunk_by([], _, Res) -> Res;
chunk_by(List, Fn, Res) ->
    Chunk = lists:takewhile(Fn, List),
    Remaining = lists:dropwhile(Fn, List),
    if
        length(Remaining) > 0 -> chunk_by(tl(Remaining), Fn, Res ++ [Chunk]);
        true -> Res ++ [Chunk]
    end.

parse2(Input) ->
    lists:filter(fun(X) -> length(X) > 0 end,
        string:split(Input, "\n", all)).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    {Numbers, Ops} = split_last(parse2(Input)),
    TransposedNumbers = transpose(lists:reverse(Numbers)),
    Chunks = chunk_by(TransposedNumbers, fun(X) -> string:trim(X) /= "" end),
    lists:sum(solve(parse_line(Ops), Chunks)).
