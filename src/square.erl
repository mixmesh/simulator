-module(square).
-export([get_area/0, get_location_index/0, get_location_generator/1,
         neighbour_distance/0]).
-export([send_simulated_messages/1]).
-export([send_messages/1]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/log.hrl").
-include_lib("player/include/player_serv.hrl").
-include("simulator_location.hrl").

-define(LOCATION, stolofsgatan).
-define(PADDING, 0.5).

%% Exported get_area

get_area() ->
    #simulator_location{area = Area} = simulator_location:get(?LOCATION),
    Area.

%% Exported: get_location_index

get_location_index() ->
    To =
        case os:getenv("NPLAYER") of
            false ->
                100;
            ToString ->
                ?l2i(ToString)
        end,
    Location = simulator_location:get(?LOCATION),
    get_location_index(1, To, Location, math:pi() * 2 / 128).

get_location_index(From, To, _Location, _DeltaAngle) when From > To ->
    [];
get_location_index(From, To,
                   #simulator_location{
                      area = {MinLongitude, _MaxLongitude,
                              MinLatitude, _MaxLatitude} = Area,
                      width_in_degrees = WidthInDegrees,
                      height_in_degrees = HeightInDegrees} = Location,
                   DeltaAngle) ->
    Label = ?l2b([<<"p">>, ?i2b(From)]),
    TimestampInSeconds = rand:uniform(),
    PaddedWidthInDegrees = WidthInDegrees * ?PADDING,
    PaddedHeightInDegrees = HeightInDegrees * ?PADDING,
    RandomLongitude = rand:uniform() * PaddedWidthInDegrees,
    RandomLatitude = rand:uniform() * PaddedHeightInDegrees,
    OrigoLongitude =
        MinLongitude + ((WidthInDegrees - PaddedWidthInDegrees) / 2) +
        RandomLongitude,
    OrigoLatitude =
        MinLatitude + ((HeightInDegrees - PaddedHeightInDegrees) / 2) +
        RandomLatitude,
    Radius = max_radius(Area, OrigoLongitude, OrigoLatitude),
    StartAngle = math:pi() * 2 * rand:uniform(),
    {UpdatedStartAngle, UpdatedDeltaAngle} =
        case rand:uniform(2) == 2 of
            true ->
                {StartAngle, DeltaAngle};
            false ->
                {-1 * StartAngle, -1 * DeltaAngle}
        end,
    Longitude = math:cos(UpdatedStartAngle) * Radius + OrigoLongitude,
    Latitude = math:sin(UpdatedStartAngle) * Radius + OrigoLatitude,
    [{Label, From,
      {Location,
       0, TimestampInSeconds,
       OrigoLongitude, OrigoLatitude,
       UpdatedStartAngle, UpdatedDeltaAngle, Radius,
       Longitude, Latitude, 0}}|
     get_location_index(From + 1, To, Location, UpdatedDeltaAngle)].

max_radius({MinLongitude, MaxLongitude, MinLatitude, MaxLatitude},
           Longitude, Latitude) ->
    min(min(Longitude - MinLongitude, MaxLongitude - Longitude),
        min(Latitude - MinLatitude, MaxLatitude - Latitude)) *
        ((rand:uniform() / 2) + 0.5).

%% Exported: get_location_generator

get_location_generator(
  {#simulator_location{meters_per_degree = MetersPerDegree,
                       update_frequency = UpdateFrequency} = Location,
   N, Timestamp,
   OrigoLongitude, OrigoLatitude,
   Angle, DeltaAngle, Radius,
   Longitude, Latitude, TotalDistance}) ->
    fun() ->
            UpdatedTimestamp = Timestamp + 1 / UpdateFrequency,
            UpdatedAngle = Angle + DeltaAngle,
            CappedAngle =
                case abs(UpdatedAngle) > math:pi() * 2 of
                    true ->
                        0;
                    false ->
                        UpdatedAngle
                end,
            NewLongitude = math:cos(CappedAngle) * Radius + OrigoLongitude,
            NewLatitude = math:sin(CappedAngle) * Radius + OrigoLatitude,
            UpdatedTotalDistance =
                TotalDistance +
                locationlib:distance(Longitude, Latitude, NewLongitude,
                                     NewLatitude) * MetersPerDegree,
            io:format("m/s: ~w\n", [UpdatedTotalDistance / UpdatedTimestamp]),
            {{UpdatedTimestamp, NewLongitude, NewLatitude},
             get_location_generator(
               {Location,
                N + 1, UpdatedTimestamp,
                OrigoLongitude, OrigoLatitude,
                CappedAngle, DeltaAngle, Radius,
                NewLongitude, NewLatitude, UpdatedTotalDistance})}
    end.

%% Exported: neighbour_distance

neighbour_distance() ->
    Location = simulator_location:get(?LOCATION),
    Location#simulator_location.neighbour_distance_in_degrees.

%% Exported: send_simulated_messages

send_simulated_messages(Players) ->
    timer:apply_after(15000, ?MODULE, send_messages, [Players]).

%% Exported: send_messages

send_messages(Players) ->
    lists:foreach(
      fun(#player{nym = Nym, player_serv_pid = PlayerServPid}) ->
              ?daemon_log_fmt("Send message to ~s", [Nym]), 
              Payload = ?i2b(erlang:unique_integer([positive])),
              ok = player_serv:send_message(PlayerServPid, <<"p1">>, Payload)
      end, Players).
