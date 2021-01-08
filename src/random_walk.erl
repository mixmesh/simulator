-module(random_walk).
-export([get_area/0, get_location_index/0, get_location_generator/1,
         neighbour_distance/0]).
-export([send_simulated_messages/1]).
-export([send_messages/2]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/log.hrl").
-include_lib("player/include/player_serv.hrl").
-include("simulator_location.hrl").

-define(LOCATION, stolofsgatan).
-define(RESEND_MESSAGES_TIME, 60000).

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
    get_location_index(1, To, scale_factor(), Location).

scale_factor() ->
    case os:getenv("SCALEFACTOR") of
        false ->
            1;
        ScaleFactorString ->
            ?l2i(ScaleFactorString)
    end.

get_location_index(From, To, _ScaleFactor, _Location) when From > To ->
    [];
get_location_index(From, To,
                   ScaleFactor,
                   #simulator_location{
                      area = {MinLongitude, _MaxLongitude,
                              MinLatitude, _MaxLatitude},
                      width_in_degrees = WidthInDegrees,
                      height_in_degrees = HeightInDegrees} = Location) ->
    Label = ?l2b([<<"p">>, ?i2b(From)]),
    TimestampInSeconds = rand:uniform(),
    NextLongitudeDelta = fun() -> noise(0.05) end,
    NextLatitudeDelta = fun() -> noise(0.05) end,
    Longitude = MinLongitude + WidthInDegrees * rand:uniform(),
    Latitude = MinLatitude + HeightInDegrees * rand:uniform(),
    LongitudeDirection = random_direction(),
    LatitudeDirection = random_direction(),
    [{Label, From,
      {ScaleFactor,
       Location,
       0, TimestampInSeconds,
       MinLongitude, MinLatitude,
       NextLongitudeDelta, NextLatitudeDelta,
       Longitude, Latitude,
       LongitudeDirection, LatitudeDirection, 0}}|
     get_location_index(From + 1, To, ScaleFactor, Location)].

random_direction() ->
    case rand:uniform() > 0.5 of
        true ->
            1;
        false ->
            -1
    end.

%% https://en.wikipedia.org/wiki/Perlin_noise
noise(Step) ->
    A = rand:uniform(),
    B = rand:uniform(),
    {A, fun() -> noise(Step, Step, A, B) end}.

noise(1.0, Step, _A, B) ->
    {B, fun() -> noise(Step, Step, B, rand:uniform()) end};
noise(Travel, Step, _A, B) when Travel > 1 ->
    NextB = rand:uniform(),
    InterpolatedB = smoothstep(B, NextB, 1 - Travel),
    {InterpolatedB, fun() -> noise(Step - (1 - Travel), Step, B, NextB) end};
noise(Travel, Step, A, B) ->
    InterpolatedB = smoothstep(A, B, Travel),
    {InterpolatedB, fun() -> noise(Travel + Step, Step, A, B) end}.

%% https://en.wikipedia.org/wiki/Smoothstep
smoothstep(A, B, W) ->
    (B - A) * (3.0 - W * 2.0) * W * W + A.

%% Exported: get_location_generator

get_location_generator(
  {ScaleFactor,
   #simulator_location{width_in_degrees = WidthInDegrees,
                       height_in_degrees = HeightInDegrees,
                       meters_per_degree = MetersPerDegree,
                       update_frequency = UpdateFrequency,
                       degrees_per_update = DegreesPerUpdate} =
       Location,
   N, Timestamp,
   MinLongitude, MinLatitude,
   NextLongitudeDelta, NextLatitudeDelta,
   Longitude, Latitude,
   LongitudeDirection, LatitudeDirection, TotalDistance}) ->
    fun() ->
            UpdatedTimestamp = Timestamp + (1 / UpdateFrequency / ScaleFactor),
            {LongitudeDelta, EvenNextLongitudeDelta} = NextLongitudeDelta(),
            LongitudeMovement =
                DegreesPerUpdate * LongitudeDelta - DegreesPerUpdate / 2,
            {NewLongitude, NewLongitudeDirection} =
                maybe_change_direction(
                  MinLongitude, Longitude, LongitudeDirection,
                  LongitudeMovement, WidthInDegrees),
            {LatitudeDelta, EvenNextLatitudeDelta} = NextLatitudeDelta(),
            LatitudeMovement =
                DegreesPerUpdate * LatitudeDelta - DegreesPerUpdate / 2,
            {NewLatitude, NewLatitudeDirection} =
                maybe_change_direction(
                  MinLatitude, Latitude, LatitudeDirection, LatitudeMovement,
                  HeightInDegrees),
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
                MinLongitude, MinLatitude,
                EvenNextLongitudeDelta, EvenNextLatitudeDelta,
                NewLongitude, NewLatitude,
                NewLongitudeDirection, NewLatitudeDirection,
                UpdatedTotalDistance})}
    end.

maybe_change_direction(MinCoordinate, Coordinate, Direction, Movement, Side) ->
    case Coordinate + Direction * Movement of
        NewCoordinate when NewCoordinate > MinCoordinate andalso
                           NewCoordinate < MinCoordinate + Side ->
            {NewCoordinate, Direction};
        _ ->
            NewDirection = -Direction,
            {Coordinate + NewDirection * Movement, NewDirection}
    end.

%% Exported: neighbour_distance

neighbour_distance() ->
    Location = simulator_location:get(?LOCATION),
    Location#simulator_location.neighbour_distance_in_degrees.

%% Exported: send_simulated_messages

send_simulated_messages(Players) ->
    timer:apply_after(5000, ?MODULE, send_messages,
                      [Players, scale_factor()]).

%% Exported: send_messages

send_messages(Players, ScaleFactor) ->
    lists:foreach(
      fun(#player{nym = _Nym, player_serv_pid = PlayerServPid}) ->
              Payload = ?i2b(erlang:unique_integer([positive])),
              spawn(
                fun() ->
                        io:format(">"),
                        player_serv:send_message(PlayerServPid, <<"p1">>,
                                                 Payload),
                        io:format("<")
                end)
      end, Players),
    timer:apply_after(trunc(?RESEND_MESSAGES_TIME / ScaleFactor), ?MODULE,
                      send_messages, [Players, ScaleFactor]).
