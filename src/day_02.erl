-module(day_02).
-export([part1/1, part2/1]).

parse(Input) ->
    Pairs = string:split(Input, ",", all),
    TrimmedPairs = lists:map(fun string:trim/1, Pairs),
    NonEmptyPairs = lists:filter(fun(X) -> length(X) > 0 end, TrimmedPairs),
    Ranges = lists:map(fun(Pair) -> string:split(Pair, "-", all) end, NonEmptyPairs),
    lists:map(fun([A, B]) -> {range, list_to_integer(A), list_to_integer(B)} end, Ranges).

expand([], ExpandedRanges) -> ExpandedRanges;
expand([{range, A, B}|Ranges], ExpandedRanges) ->
    ExpandedRange = lists:map(fun integer_to_list/1, lists:seq(A, B)),
    expand(Ranges, ExpandedRanges ++ [ExpandedRange]).

chunk(_, ChunkSize, _) when ChunkSize =< 0 -> error("ChunkSize must be > 0");
chunk(List, ChunkSize, Res) when length(List) =< ChunkSize -> Res ++ [List];
chunk(List, ChunkSize, Res) ->
    Chunk = lists:sublist(List, 1, ChunkSize),
    Remaining = lists:sublist(List, ChunkSize + 1, length(List)),
    chunk(Remaining, ChunkSize, Res ++ [Chunk]).

repeated_digits(_, 0) -> false;
repeated_digits([D1|_] = Digits, 1) -> lists:all(fun(X) -> X == D1 end, Digits);
repeated_digits(Digits, Split) when length(Digits) rem Split == 0 ->
    [Chunk|_] = Chunks = chunk(Digits, Split, []),
    Heads = lists:map(fun([H|_]) -> H end, Chunks),
    LeadingZeroes = lists:any(fun(H) -> H == $0 end, Heads),
    not LeadingZeroes and lists:all(fun(C) -> C == Chunk end, Chunks);
repeated_digits(_, _) -> false.

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    Ranges = parse(Input),
    Ids = lists:append(expand(Ranges, [])),
    EvenLengthIds = lists:filter(fun(X) -> length(X) rem 2 == 0 end, Ids),
    RepeatIds = lists:filter(fun(X) -> repeated_digits(X, length(X) div 2) end, EvenLengthIds),
    lists:sum(lists:map(fun list_to_integer/1, RepeatIds)).

repeated_digits_rec(_, 0) -> false;
repeated_digits_rec(Digits, Split) ->
    Repeated = repeated_digits(Digits, Split),
    Repeated or repeated_digits_rec(Digits, Split - 1).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    Ranges = parse(Input),
    Ids = lists:append(expand(Ranges, [])),
    RepeatIds = lists:filter(fun(X) -> repeated_digits_rec(X, length(X) div 2) end, Ids),
    lists:sum(lists:map(fun list_to_integer/1, RepeatIds)).
