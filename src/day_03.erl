-module(day_03).
-export([part1/1, part2/1]).

parseBattery(Line) ->
    lists:map(fun(B) -> list_to_integer([B]) end, Line).

parse(Input) ->
    Lines = string:split(Input, "\n", all),
    TrimmedLines = lists:map(fun string:trim/1, Lines),
    NonEmptyLines = lists:filter(fun(X) -> length(X) > 0 end, TrimmedLines),
    lists:map(fun parseBattery/1, NonEmptyLines).

max_voltage([], Voltage, _) -> Voltage;
max_voltage(_, Voltage, 0) -> Voltage;
max_voltage(Batteries, Voltage, Remaining) when length(Batteries) == Remaining ->
    RestVoltage = lists:foldl(fun(X, Sum) -> Sum * 10 + X end, 0 , Batteries),
    TotalVoltage = Voltage * math:pow(10, length(Batteries)) + RestVoltage,
    trunc(TotalVoltage);
max_voltage(Batteries, Voltage, Remaining) when length(Batteries) > Remaining ->
    WindowSize = length(Batteries) - Remaining,
    MaxVolt = lists:max(lists:sublist(Batteries, 1, WindowSize)),
    [_|Rest] = lists:dropwhile(fun(B) -> B /= MaxVolt end, Batteries),
    max(
        % Slide window without choosing a battery
        % Any other battery we would choose from the window
        % would result in a lower voltage than the max
        % so we can skip them
        max_voltage(Rest, Voltage, Remaining),
        % Choose a battery and slide window
        max_voltage(Rest, Voltage * 10 + MaxVolt, Remaining - 1)
    ).

part({file, Input}, BatteryCount) ->
    {ok, Content} = file:read_file(Input),
    part({string, binary_to_list(Content)}, BatteryCount);
part({string, Input}, BatteryCount) ->
    Batteries = parse(Input),
    lists:sum(
        lists:map(fun(B) -> max_voltage(B, 0, BatteryCount) end,
            Batteries)).

part1(Input) -> part(Input, 2).
part2(Input) -> part(Input, 12).
