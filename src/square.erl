-module(square).
-export([get_location/0, get_location_index/0, get_location_generator/1,
         neighbour_distance/0, send_simulated_messages/1, center_target/0]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/log.hrl").
-include_lib("player/include/player_serv.hrl").
-include("simulator_location.hrl").

-define(LOCATION, stolofsgatan).
-define(DEFAULT_NUMBER_OF_PLAYERS, 100).
-define(INITIAL_DELAY, 5000).
-define(RESEND_TIME, (60000 * 4)).
-define(TARGET_NYM, <<"p1">>).

-define(PADDING, 0.5).

%%
%% Exported: get_location
%%

get_location() ->
    simulator_location:get(?LOCATION).

%%
%% Exported: get_location_index
%%

get_location_index() ->
    To = simulator:nplayer(?DEFAULT_NUMBER_OF_PLAYERS),
    get_location_index(1, To, simulator:scale_factor(), get_location(),
                       math:pi() * 2 / 128).

get_location_index(From, To, _ScaleFactor, _Location, _DeltaAngle)
  when From > To ->
    [];
get_location_index(From, To, ScaleFactor,
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
      {ScaleFactor,
       Location,
       0, TimestampInSeconds,
       OrigoLongitude, OrigoLatitude,
       UpdatedStartAngle, UpdatedDeltaAngle, Radius,
       Longitude, Latitude, 0}}|
     get_location_index(From + 1, To, ScaleFactor, Location,
                        UpdatedDeltaAngle)].

max_radius({MinLongitude, MaxLongitude, MinLatitude, MaxLatitude},
           Longitude, Latitude) ->
    min(min(Longitude - MinLongitude, MaxLongitude - Longitude),
        min(Latitude - MinLatitude, MaxLatitude - Latitude)) *
        ((rand:uniform() / 2) + 0.5).

%%
%% Exported: get_location_generator
%%

get_location_generator(
  {ScaleFactor,
   #simulator_location{meters_per_degree = MetersPerDegree,
                       update_frequency = UpdateFrequency} = Location,
   N, Timestamp,
   OrigoLongitude, OrigoLatitude,
   Angle, DeltaAngle, Radius,
   Longitude, Latitude, TotalDistance}) ->
    fun() ->
            UpdatedTimestamp = Timestamp + (1 / UpdateFrequency / ScaleFactor),
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
%            io:format("m/s: ~w\n",
%                      [UpdatedTotalDistance /
%                           (UpdatedTimestamp * ScaleFactor)]),
            {{UpdatedTimestamp, NewLongitude, NewLatitude},
             get_location_generator(
               {ScaleFactor,
                Location,
                N + 1, UpdatedTimestamp,
                OrigoLongitude, OrigoLatitude,
                CappedAngle, DeltaAngle, Radius,
                NewLongitude, NewLatitude, UpdatedTotalDistance})}
    end.

%%
%% Exported: neighbour_distance
%%

neighbour_distance() ->
    Location = get_location(),
    Location#simulator_location.neighbour_distance_in_degrees.

%%
%% Exported: send_simulated_messages
%%

send_simulated_messages(Players) ->
    ScaleFactor = simulator:scale_factor(),
    timer:apply_after(trunc(?INITIAL_DELAY / ScaleFactor),
                      simulator, send_messages,
                      [Players, ScaleFactor, ?TARGET_NYM, ?RESEND_TIME]).

%%
%% Exported: center_target
%%

center_target() ->
    {true, ?TARGET_NYM}.
