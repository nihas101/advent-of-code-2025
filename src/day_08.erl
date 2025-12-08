-module(day_08).
-export([part1/1, part1/2, part2/1]).

parse_line(L) ->
    [X, Y, Z] = string:split(L, ",", all),
    {pos, list_to_integer(X), list_to_integer(Y), list_to_integer(Z)}.

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    [parse_line(L) || L <- NonEmptyLines].

euclidian_distance_squared({pos, PX, PY, PZ}, {pos, QX, QY, QZ}) ->
    DX = (PX - QX),
    DY = (PY - QY),
    DZ = (PZ - QZ),
    (DX * DX) + (DY * DY) + (DZ * DZ).

distance_map([P|T]) -> distance_map(P, T, T, #{}).
distance_map(_, [], [], DistanceMap) -> DistanceMap;
distance_map(_, [], [P|T], DistanceMap) -> distance_map(P, T, T, DistanceMap);
distance_map(P, [Q|T], S, DistanceMap) ->
    Dist = euclidian_distance_squared(P,Q),
    NewDistanceMap = DistanceMap#{ {P,Q} => Dist },
    distance_map(P, T, S, NewDistanceMap).

connect(_, Circuits, 0) -> Circuits;
connect([{{P, Q}, _} | T], Circuits, ConnectionsCount) ->
    {ConnectingCircuits, DisconnectedCircuits} = lists:partition(fun(C) -> sets:is_element(P, C) or sets:is_element(Q, C) end, Circuits),
    NewCircuit = sets:union(ConnectingCircuits),
    connect(T, [NewCircuit] ++ DisconnectedCircuits, ConnectionsCount - 1).

part1({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part1({string, binary_to_list(Content)}, 1000).
part1({string, Input}, ConnectionsCount) ->
    JunctionBoxes = parse(Input),
    DistanceMap = distance_map(JunctionBoxes),
    SortedJunctionBoxes = lists:sort(fun({_, DistA},{_, DistB}) -> DistA < DistB end, maps:to_list(DistanceMap)),
    InitialConnections = lists:map(fun(JB) -> sets:from_list([JB]) end, JunctionBoxes),
    ConnectedJunctionBoxes = connect(SortedJunctionBoxes, InitialConnections, ConnectionsCount),
    CircuitSizes = lists:map(fun(CJB) -> length(sets:to_list(CJB)) end, ConnectedJunctionBoxes),
    [A, B, C | _] = lists:sort(fun(A, B) -> A > B end, CircuitSizes),
    A * B * C.

connect_all(_, Circuits, Previous) when length(Circuits) == 1 -> Previous;
connect_all([{{P, Q}, _} | T], Circuits, _) ->
    {ConnectingCircuits, DisconnectedCircuits} = lists:partition(fun(C) -> sets:is_element(P, C) or sets:is_element(Q, C) end, Circuits),
    NewCircuit = sets:union(ConnectingCircuits),
    connect_all(T, [NewCircuit] ++ DisconnectedCircuits, {P, Q}).

part2({file, Input}) ->
    {ok, Content} = file:read_file(Input),
    part2({string, binary_to_list(Content)});
part2({string, Input}) ->
    JunctionBoxes = parse(Input),
    DistanceMap = distance_map(JunctionBoxes),
    SortedJunctionBoxes = lists:sort(fun({_, DistA},{_, DistB}) -> DistA < DistB end, maps:to_list(DistanceMap)),
    InitialConnections = lists:map(fun(JB) -> sets:from_list([JB]) end, JunctionBoxes),
    {{pos, X1, _, _}, {pos, X2, _, _}} = connect_all(SortedJunctionBoxes, InitialConnections, {}),
    X1 * X2.
