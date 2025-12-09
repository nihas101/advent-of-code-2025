-module(day_09).
-export([part1/1, part2/1]).

parse_line(L) ->
    [X, Y] = string:split(L, ",", all),
    {pos, list_to_integer(X), list_to_integer(Y)}.

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    [parse_line(L) || L <- NonEmptyLines].

area({pos, PX, PY}, {pos, QX, QY}) ->
    (abs(PX - QX) + 1) * (abs(PY - QY) + 1).

area_map([P|T]) -> area_map(P, T, T, #{}).
area_map(_, [], [], DistanceMap) -> DistanceMap;
area_map(_, [], [P|T], DistanceMap) -> area_map(P, T, T, DistanceMap);
area_map(P, [Q|T], S, DistanceMap) ->
    Area = area(P,Q),
    NewDistanceMap = DistanceMap#{ {P,Q} => Area },
    area_map(P, T, S, NewDistanceMap).

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)});
part1({string, Input}) ->
    Tiles = parse(Input),
    AreaMap = area_map(Tiles),
    [{_, MaxArea} | _] = lists:sort(fun({_, AreaA},{_, AreaB}) -> AreaA > AreaB end, maps:to_list(AreaMap)),
    MaxArea.

normalize({pos, PX, PY}, {pos, QX, QY}) ->
    MinX = min(PX, QX),
    MaxX = max(PX, QX),
    MinY = min(PY, QY),
    MaxY = max(PY, QY),
    {{pos, MinX, MinY}, {pos, MaxX, MaxY}}.
normalize({P, Q}) -> normalize(P, Q).

loop([]) -> [];
loop(Points) ->
    Pairs = lists:zip(Points, tl(Points) ++ [hd(Points)]),
    lists:map(fun normalize/1, Pairs).

intersect_fun({{pos, PX, PY}, {pos, QX, QY}}) ->
    fun({{pos, PolX, PolY}, {pos, QolX, QolY}}) ->
        % Are the rectangles intersecting?
        PolX < QX andalso PolY < QY
        andalso QolX > PX andalso QolY > PY
    end.

intersects({P, Q}, Polygon) ->
    NormalizedCandidate = normalize(P, Q),
    lists:any(intersect_fun(NormalizedCandidate), Polygon).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    Tiles = parse(Input),
    AreaMap = area_map(Tiles),
    Candidates = lists:sort(fun({_, AreaA},{_, AreaB}) -> AreaA > AreaB end, maps:to_list(AreaMap)),
    Polygon = loop(Tiles),
    [{_, MaxArea} | _] = lists:dropwhile(fun({C, _}) -> intersects(C, Polygon) end, Candidates),
    MaxArea.
