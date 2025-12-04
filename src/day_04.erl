-module(day_04).
-export([part1/1, part2/1]).

parseLine([], _, _, Paper) -> Paper;
parseLine([$@|T], X, Y, Paper) ->
    parseLine(T, X+1, Y, Paper ++ [{pos, X, Y}]);
parseLine([$.|T], X, Y, Paper) -> parseLine(T, X+1, Y, Paper).

parseLines([], _, Paper) -> Paper;
parseLines([L|Ls], Y, Paper) ->
    parseLines(Ls, Y+1, Paper ++ parseLine(L, 0, Y, [])).

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    parseLines(NonEmptyLines, 0, []).

neighbours({pos, X, Y}) ->
    [
        {pos, X-1, Y-1}, {pos, X, Y-1}, {pos, X+1, Y-1},
        {pos, X-1,   Y},                {pos, X+1,   Y},
        {pos, X-1, Y+1}, {pos, X, Y+1}, {pos, X+1, Y+1}
    ].

surrounding_paper_count({pos, X, Y}, Set) ->
    Neighbours = neighbours({pos, X, Y}),
    SurroundingPaper = lists:filter(fun(N) -> sets:is_element(N, Set) end, Neighbours),
    length(SurroundingPaper).

removable_papers(PaperPositions) ->
    PaperSet = sets:from_list(PaperPositions),
    PP = [{P, surrounding_paper_count(P, PaperSet)} || P <- PaperPositions],
    Removable = lists:filter(fun({_, C}) -> C < 4 end, PP),
    [P || {P,_} <- Removable].

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    PaperPositions = parse(Input),
    Removable = removable_papers(PaperPositions),
    length(Removable).

part2(_, [], Removed) -> Removed;
part2(PaperPositions, Removable, Removed) ->
    NewPaperPositions = lists:subtract(PaperPositions, Removable),
    part2(NewPaperPositions,
        removable_papers(NewPaperPositions),
        Removed + length(Removable)).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    PaperPositions = parse(Input),
    part2(PaperPositions, removable_papers(PaperPositions), 0).
