-module(mesh).
-export([get_location/0, get_location_index/0, get_location_generator/1,
         neighbour_distance/0]).
-export([send_simulated_messages/1]).

-include_lib("apptools/include/log.hrl").
-include("simulator_location.hrl").

-define(LOCATION, stolofsgatan).

%% Exported: get_location

get_location() ->
    simulator_location:get(?LOCATION).

%% Exported: get_location_index

get_location_index() ->
    N = case os:getenv("NPLAYER") of
	    false -> 8;
	    NStr -> list_to_integer(NStr)
	end,
    true = (N =< 9),
    #simulator_location{
       area = {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude}} =
        get_location(),
    DeltaLongitude = (MaxLongitude - MinLongitude) / (N + 2),
    DeltaLatitude = (MaxLatitude - MinLatitude) / (N + 2),
    OffsetLongitude = 0, %% DeltaLongitude/15,
    OffsetLatitude = 0, %% DeltaLatitude/15,
    [{mesh_point(I, J), (I - 1) * N + J,
      {MinLongitude + I * DeltaLongitude + OffsetLongitude,
       MinLatitude + J * DeltaLatitude + OffsetLatitude, 0, 1}} || 
	I <- lists:seq(N, 1, -1), J <- lists:seq(N, 1, -1)].

mesh_point(I, J) ->
    iolist_to_binary(io_lib:format("p~w~w", [I, J])).

%% Exported: get_location_generator

get_location_generator({Longitude, Latitude, Timestamp, TimeStep}) ->
    fun() ->
	    NextTimestamp = Timestamp + TimeStep,
	    {{NextTimestamp, Longitude, Latitude},
	     get_location_generator(
               {Longitude, Latitude, NextTimestamp, TimeStep})}
    end.

%% Exported: neighbour_distance

neighbour_distance() ->
    N = case os:getenv("NPLAYER") of
            false -> 8;
            NStr -> list_to_integer(NStr)
        end,
    #simulator_location{
       area = {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude}} =
        get_location(),
    DeltaLongitude = (MaxLongitude - MinLongitude)/ (N + 2),
    DeltaLatitude = (MaxLatitude - MinLatitude) / (N + 2),
    max(DeltaLongitude, DeltaLatitude) * 1.1.

%% Exported: send_simulated_messages

send_simulated_messages(_Players) ->
    nyi.
