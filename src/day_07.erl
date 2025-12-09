-module(day_07).
-export([part1/1, part2/1]).

parse_line([], _, _, Splitters) -> Splitters;
parse_line([$S|T], X, Y, Splitters) ->
    parse_line(T, X+1, Y, Splitters ++ [{start, X, Y}]);
parse_line([$^|T], X, Y, Splitters) ->
    parse_line(T, X+1, Y, Splitters ++ [{splitter, X, Y}]);
parse_line([$.|T], X, Y, Splitters) -> parse_line(T, X+1, Y, Splitters).

parse_lines(Ls) -> parse_lines(Ls, 0, []).
parse_lines([], _, Splitters) ->
    Start = lists:keyfind(start, 1, Splitters),
    MaxYPos = lists:max([Y || {splitter, _, Y} <- Splitters]),
    {tachyons, Start, MaxYPos, Splitters};
parse_lines([L|Ls], Y, Splitters) ->
    parse_lines(Ls, Y+1, Splitters ++ parse_line(L, 0, Y, [])).

calculate_splits({start, X, Y}, MaxYPos, Splitters) ->
    calculate_splits([{pos, X, Y}], MaxYPos, Splitters, {0, sets:new()}).
calculate_splits([], _, _, {Splits, _}) -> Splits;
calculate_splits([{pos, _, Y} | Pos], MaxYPos, Splitters, Beams) when Y > MaxYPos -> calculate_splits(Pos, MaxYPos, Splitters, Beams);
calculate_splits([{pos, X, Y} = P | Pos], MaxYPos, Splitters, {Splits, Beams} = Res) ->
    AlreadyVisited = sets:is_element(P, Beams),
    IsSplitter = sets:is_element({splitter, X, Y}, Splitters),
    if
        AlreadyVisited -> calculate_splits(Pos, MaxYPos, Splitters, Res);
        IsSplitter ->
            calculate_splits([{pos, X-1, Y}, {pos, X+1, Y}] ++ Pos, MaxYPos, Splitters, {Splits + 1, Beams});
        true ->
            calculate_splits([{pos, X, Y+1}] ++ Pos, MaxYPos, Splitters, {Splits, sets:add_element(P, Beams)})
    end.

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    parse_lines(NonEmptyLines).

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    {tachyons, Start, MaxYPos, Splitters} = parse(Input),
    calculate_splits(Start, MaxYPos, sets:from_list(Splitters)).

calculate_new_beams({{_, XX, _}, TimelineValue}, Beams) ->
    PlusTimeValue = fun(CurrValue) -> CurrValue + TimelineValue end,
    NewBeams = maps:update_with(XX-1, PlusTimeValue, TimelineValue, Beams),
    maps:update_with(XX+1, PlusTimeValue, TimelineValue, NewBeams).

calculate_timelines(Beams, MaxYPos, MaxYPos, _) -> Beams;
calculate_timelines(Beams, Y, MaxYPos, Splitters) ->
    RelevantSplitters = lists:filter(fun({_, XX, YY}) -> (YY == Y) andalso maps:is_key(XX, Beams) end, Splitters),
    Timelines = lists:map(fun({_, XX, _}) -> maps:get(XX, Beams) end, RelevantSplitters),
    BeamsRemoved = lists:foldl(fun({_, XX, _}, M) -> maps:remove(XX, M) end, Beams, RelevantSplitters),
    NewBeams = lists:foldl(fun calculate_new_beams/2, BeamsRemoved, lists:zip(RelevantSplitters, Timelines)),
    calculate_timelines(NewBeams, Y+1, MaxYPos, Splitters).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    {tachyons, {start, X, _}, MaxYPos, Splitters} = parse(Input),
    Timelines = calculate_timelines(#{X => 1}, 1, MaxYPos+1, Splitters),
    lists:sum(maps:values(Timelines)).
