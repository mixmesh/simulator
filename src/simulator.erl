-module(simulator).
-export([start/0]).
-export([initialize/5]).
-export([add_players/1, update_players/1]).
-export([generate_area/3]).
-export([get_location_generator/2, get_location_index/2]).
-export([degrees_to_meters/2, meters_to_degrees/2]).
-export([width_height_in_meters/4]).
-export([zscoring/3]).
-export([binary_to_number/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

-define(WINDOW_WIDTH, 1000.0).
-define(WINDOW_HEIGHT, 1000.0).

-define(EARTH_RADIUS, 6357000).

-define(IS_NOTHING, 0).
-define(IS_FORWARDER, 1).
-define(IS_SOURCE, 2).
-define(IS_TARGET, 3).

%% Exported: start

start() ->
    ok = application:ensure_all_started(simulator).

%% Exported: initialize

initialize(MinLongitude, MaxLongitude,
           MinLatitude, MaxLatitude,
           NeighbourDistance) ->
    simulator_nif:initialize(MinLongitude, MaxLongitude,
                             MinLatitude, MaxLatitude,
                             NeighbourDistance).

%% Exported: add_players

add_players([]) ->
    ok;
add_players(NewPlayers) ->
    ConvertedNewPlayers =
        lists:map(fun({Name, X, Y}) ->
                          {?b2l(Name), X, Y}
                  end, NewPlayers),
    simulator_nif:add_players(ConvertedNewPlayers).

%% Exported: update_players

update_players([]) ->
    ok;
update_players(UpdatedPlayers) ->
    ConvertedUpdatedPlayers =
        lists:map(fun({Name, UpdatedValues}) ->
                          {?b2l(Name), convert_updated_values(UpdatedValues)}
                  end, UpdatedPlayers),
    simulator_nif:update_players(ConvertedUpdatedPlayers).

convert_updated_values([]) ->
    [];
convert_updated_values([{neighbours, Neighbours}|Rest]) ->
    UpdatedNeighbours = lists:map(fun(Name) -> ?b2l(Name) end, Neighbours),
    [{neighbours, UpdatedNeighbours}|convert_updated_values(Rest)];
convert_updated_values([{is_zombie, true}|Rest]) ->
    [{is_zombie, 1}|convert_updated_values(Rest)];
convert_updated_values([{is_zombie, false}|Rest]) ->
    [{is_zombie, 0}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, is_nothing}|Rest]) ->
    [{pick_mode, ?IS_NOTHING}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode,
                         {is_forwarder, {message_not_in_buffer, _}}}|Rest]) ->
    [{pick_mode, ?IS_NOTHING}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode,
                         {is_forwarder, {message_in_buffer, _}}}|Rest]) ->
    [{pick_mode, ?IS_FORWARDER}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, {is_source, {_, _}}}|Rest]) ->
    [{pick_mode, ?IS_SOURCE}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, {is_target, _}}|Rest]) ->
    [{pick_mode, ?IS_TARGET}|convert_updated_values(Rest)];
convert_updated_values([UpdatedValue|Rest]) ->
    [UpdatedValue|convert_updated_values(Rest)].

%% Exported: generate_area

generate_area(Index, ParseLine, KeepDb) ->
  Db = ets:new(player_db, [ordered_set]),
  Range = load_all_players(Index, ParseLine, Db, -1, {-1, -1, -1, -1, -1, -1}),
  if
      KeepDb ->
          {Db, Range};
      true ->
          Range
  end.

load_all_players(Index, _ParseLine, _Db, N, Range)
  when N == 0 orelse Index == [] ->
    Range;
load_all_players([{Name, Path}|Rest], ParseLine, Db, N, Range) ->
    io:format("Parsing ~s...\n", [Path]),
    {ok, File} = file:open(Path, [read, raw, read_ahead, binary]),
    UpdatedRange = load_player(ParseLine, Db, Name, File, Range),
    ok = file:close(File),
    load_all_players(Rest, ParseLine, Db, N - 1, UpdatedRange).

load_player(ParseLine, Db, Name, File,
            {MinTimestamp, MaxTimestamp,
             MinLongitude, MaxLongitude,
             MinLatitude, MaxLatitude} = Range) ->
    case file:read_line(File) of
        eof ->
            Range;
        {ok, Line} ->
            {Longitude, Latitude, Timestamp} = ParseLine(Line),
            UpdatedMinTimestamp =
                cswap(fun erlang:'<'/2, Timestamp, MinTimestamp),
            UpdatedMaxTimestamp =
                cswap(fun erlang:'>'/2, Timestamp, MaxTimestamp),
            UpdatedMinLongitude =
                cswap(Db, Name, min_longitude, fun erlang:'<'/2, Longitude,
                      MinLongitude),
            UpdatedMaxLongitude =
                cswap(Db, Name, max_longitude, fun erlang:'>'/2, Longitude,
                      MaxLongitude),
            UpdatedMinLatitude =
                cswap(Db, Name, min_latitude, fun erlang:'<'/2, Latitude,
                      MinLatitude),
            UpdatedMaxLatitude =
                cswap(Db, Name, max_latitude, fun erlang:'>'/2, Latitude,
                      MaxLatitude),
            load_player(ParseLine, Db, Name, File,
                        {UpdatedMinTimestamp, UpdatedMaxTimestamp,
                         UpdatedMinLongitude, UpdatedMaxLongitude,
                         UpdatedMinLatitude, UpdatedMaxLatitude})
    end.

cswap(Compare, Value1, Value2) ->
    case Value2 == -1 orelse Compare(Value1, Value2) of
        true ->
            Value1;
        false ->
            Value2
    end.

cswap(Db, Name, Coordinate, Compare, Value1, Value2) ->
    case Value2 == -1 orelse Compare(Value1, Value2) of
        true ->
            true = ets:insert(Db, {{Coordinate, Name}, Value1}),
            Value1;
        false ->
            case ets:match(Db, {{Coordinate, Name}, '_'}) of
                [] ->
                    true = ets:insert(Db, {{Coordinate, Name}, Value2});
                _ ->
                    Value2
            end
    end.

%% Exported: get_location_generator

get_location_generator(Path, ParseLine) ->
    {ok, File} = file:open(Path, [read, raw, read_ahead, binary]),
    get_location_generator(ParseLine, File, -1).

get_location_generator(ParseLine, File, FirstTimestamp) ->
    fun() ->
            case file:read_line(File) of
                eof ->
                    file:close(File),
                    end_of_locations;
                {ok, Line} ->
                    {{Longitude, Latitude, Timestamp}, Speedup} =
                        ParseLine(Line),
                    {UpdatedTimestamp, UpdatedFirstTimestamp} =
                        if
                            FirstTimestamp == -1 ->
                                {0, Timestamp};
                            true ->
                                {(Timestamp - FirstTimestamp) / Speedup,
                                 FirstTimestamp}
                        end,
                    {{UpdatedTimestamp, Longitude, Latitude},
                     get_location_generator(
                       ParseLine, File, UpdatedFirstTimestamp)}
            end
    end.

%% Exported: get_location_index

get_location_index([], _GenerateFilename) ->
    [];
get_location_index([Name|Rest], GenerateFilename) ->
    [{Name, GenerateFilename(Name)}|
     get_location_index(Rest, GenerateFilename)].

%% Exported: degrees_to_meters

degrees_to_meters(Degrees, {MinLongitude, MaxLongitude,
                            MinLatitude, MaxLatitude}) ->
    {Width, Height} =
        width_height_in_meters(MinLongitude, MaxLongitude,
                               MinLatitude, MaxLatitude),
    ScaleFactor =
        (Width / (MaxLongitude - MinLongitude) +
             Height / (MaxLatitude - MinLatitude)) / 2,
    ScaleFactor * Degrees.

%% Exported: meters_to_degrees

meters_to_degrees(Meters, {MinLongitude, MaxLongitude,
                           MinLatitude, MaxLatitude}) ->
    {Width, Height} =
        width_height_in_meters(MinLongitude, MaxLongitude,
                               MinLatitude, MaxLatitude),
    ScaleFactor =
        ((MaxLongitude - MinLongitude) / Width +
             (MaxLatitude - MinLatitude) / Height) / 2,
    ScaleFactor * Meters.

%% Exported: width_height_in_meters

width_height_in_meters(MinLongitude, MaxLongitude,
                       MinLatitude, MaxLatitude) ->
    width_height_in_meters(MinLatitude,
                           MaxLongitude - MinLongitude,
                           MaxLatitude - MinLatitude).

width_height_in_meters(Latitude, DeltaLongitude, DeltaLatitude) ->
    Diameter =
        math:cos(degree_to_radians(Latitude)) * ?EARTH_RADIUS * 2 * math:pi(),
    Width = DeltaLongitude / 360 * Diameter,
    Height = DeltaLatitude / 360 * 2 * ?EARTH_RADIUS * math:pi(),
    {Width, Height}.

degree_to_radians(X) ->
    X * math:pi() / 180.

%% Original by Olof Helgesson:
%% --takes degrees
%% --letitude is the latitude of origign
%% -- deltaX is the change from origo in degrees
%% function distance(latitude, deltaLat, deltaLong)
%%     local EARTH_RADIUS= 6357000 --meters
%%     local localDiameter = math.cos(degToRad(latitude)) * EARTH_RADIUS * 2 * math.pi
%%     local meterWidth = deltaLong / 360 * localDiameter
%%     local meterHeight = deltaLat / 360 * 2 * EARTH_RADIUS * math.pi
%%     return meterWidth, meterHeight
%% end

%% function degToRad(x)
%%     return x * math.pi / 180
%% end

%% print(distance(50, 5, 5))
%%  -- }356588.29232303 554752.9027464

%% Exported: zscoring

zscoring(Index, ParseLine, Stdev) ->
    {Db, _Range} = generate_area(Index, ParseLine, true),
    MinLongitudeNames = filter(zscore_coordinate(Db, min_longitude), Stdev),
    MaxLongitudeNames = filter(zscore_coordinate(Db, max_longitude), Stdev),
    MinLatitudeNames = filter(zscore_coordinate(Db, min_latitude), Stdev),
    MaxLatitudeNames = filter(zscore_coordinate(Db, max_latitude), Stdev),
    lists:sort(
      sets:to_list(
        sets:intersection(
          [sets:from_list(MinLongitudeNames),
           sets:from_list(MaxLongitudeNames),
           sets:from_list(MinLatitudeNames),
           sets:from_list(MaxLatitudeNames)]))).

zscore_coordinate(Db, Which) ->
    List =
        lists:sort(
          lists:map(fun([Name, Coordinate]) ->
                            {Name, Coordinate}
                    end, ets:match(Db, {{Which, '$1'}, '$2'}))),
    Names =
        lists:map(fun({Name, _Coordinate}) ->
                          Name
                  end, List),
    Coordinates =
        lists:map(fun({_Name, Coordinate}) ->
                          Coordinate
                  end, List),
    Zscores = zscore(Coordinates),
    lists:zip3(Names, Coordinates, Zscores).

filter([], _Stdev) ->
    [];
filter([{Name, Coordinate, Zscore}|Rest], Stdev) ->
    case abs(Zscore) < Stdev of
        true ->
            [Name|filter(Rest, Stdev)];
        false ->
            io:format("Outlier: ~p\n", [{Name, Zscore, Coordinate}]),
            filter(Rest, Stdev)
    end.

%% https://github.com/msharp/elixir-statistics/blob/master/lib/statistics.ex
zscore(List) ->
    Lmean = list_mean(List),
    Lstdev = stdev(List),
    lists:map(fun(N) ->
                      (N - Lmean) / Lstdev
              end, List).

list_mean(List) ->
    list_mean(List, 0, 0).

list_mean([], T1, L) ->
    T1 / L;
list_mean([X|Xs], T1, L) ->
    list_mean(Xs, T1 + X, L + 1).

stdev(List) ->
    math:sqrt(variance(List)).

variance(List) ->
    ListMean = list_mean(List),
    list_mean(lists:map(fun(X) ->
                                (ListMean - X) * (ListMean - X)
                        end, List)).

%% Exported: binary_to_number

binary_to_number(Binary) ->
    try
        ?b2f(Binary)
    catch
        error:badarg ->
            ?b2i(Binary)
    end.
