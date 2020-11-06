-module(mesh).
-export([get_area/0]).
-export([get_location_index/0, get_location_generator/1]).
-export([neighbour_distance_in_meters/0]).
-export([degrees_to_meters/1, meters_to_degrees/1]).

%% A one kilometer square in Silicon Valley
-define(NE1KM, {37.41509946129324, -122.06668983109768}).
-define(SW1KM, {37.406116847158806, -122.07799999999997}).

get_area() ->
    {MaxLatitude, MaxLongitude} = ?NE1KM,
    {MinLatitude, MinLongitude} = ?SW1KM,
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude}.

%% Exported: read_index

mesh_point(I,J) ->
    iolist_to_binary(io_lib:format("p~w~w", [I,J])).

get_location_index() ->
    %% fixme: read NPLAYER and vary the mesh size!
    N = 8,  %% mesh is NxN
    {MinX, MaxX, MinY, MaxY} = get_area(),
    Dx = (MaxX - MinX)/(N+2),
    Dy = (MaxY - MinY)/(N+2),
    OffsX = 0, %% Dx/15,
    OffsY = 0, %% Dy/15,
    [ {mesh_point(I,J), {MinX+I*Dx+OffsX, MinY+J*Dy+OffsY, 0, 2}} || 
	I <- lists:seq(N, 1, -1), J <- lists:seq(N, 1, -1) ].

%% Exported: get_location_generator

get_location_generator({Longitude, Latitude, Timestamp, TimeStep}) ->
    fun() ->
	    NextTimestamp = Timestamp + TimeStep,
	    {{NextTimestamp, Longitude, Latitude},
	     get_location_generator({Longitude, Latitude, 
				     NextTimestamp, TimeStep})}
    end.

neighbour_distance_in_meters() ->
    %% {MinX, MaxX, MinY, MaxY} = get_area(),
    %% Dx = (MaxX - MinX)/10,
    %% Dy = (MaxY - MinY)/10,
    %% Nx = degrees_to_meters(Dx), %% 112.58
    %% Ny = degrees_to_meters(Dy), %% 89.41
    %% D = max(Nx, Ny)
    114.

%% Exported: degrees_to_meters

degrees_to_meters(Degrees) ->
    simulator:degrees_to_meters(Degrees, get_area()).

meters_to_degrees(Meters) ->
    simulator:meters_to_degrees(Meters, get_area()).
