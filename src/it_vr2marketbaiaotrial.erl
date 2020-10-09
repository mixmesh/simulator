-module(it_vr2marketbaiaotrial).
-export([get_area/0, generate_area/0]).
-export([get_location_generator/1, get_location_index/0]).
-export([neighbour_distance_in_meters/0]).
-export([degrees_to_meters/1, meters_to_degrees/1]).
-export([width_height_in_meters/0]).

-include_lib("apptools/include/shorthand.hrl").

-define(FIRE_FIGTHERS, [<<"FF1">>,<<"FF2">>,<<"FF3">>,<<"FF4">>,<<"Truck">>]).
-define(SCALE_FACTORS,
        {512,512,41132114,-8049844,9.413861147368152,5.986396322646215}).

%% Exported: get_area

get_area() ->
    %% Generated with generate_area/0
    {-8.04984382137331,-8.04371375153892,41.13211361,41.1417534038149}.

%% Exported: generate_area

generate_area() ->
    generate_area(?FIRE_FIGTHERS).

generate_area(Names) ->
    simulator:generate_area(
      get_location_index(Names), fun parse_line/1, false).

parse_line(Line) ->
    [Latitude, Longitude, Time] = string:lexemes(Line, " "),
    LongitudeNumber = simulator:binary_to_number(Longitude),
    LatitudeNumber = simulator:binary_to_number(Latitude),
    Timestamp = to_seconds(?b2l(string:chomp(Time))),
    {LongitudeNumber, LatitudeNumber, Timestamp}.

to_seconds(Time) ->
    [Hours, Minutes, Seconds] = string:lexemes(Time, ":"),
    ?l2i(Hours) * 3600 +
        ?l2i(Minutes) * 60 +
        ?l2f(Seconds).

%% Exported: get_location_generator

get_location_generator(Path) ->
    simulator:get_location_generator(Path, fun(Line) ->
                                                   {parse_line(Line), 1.0}
                                           end).

%% Exported: get_location_index

get_location_index() ->
    get_location_index(?FIRE_FIGTHERS).

get_location_index(Names) ->
    DataDir = code:priv_dir(simulator),
    simulator:get_location_index(
      Names,
      fun(Name) ->
              filename:join(
                [DataDir,
                 <<"it_vr2marketbaiaotrial">>,
                 ?l2b([<<"data">>, Name, <<".txt">>])])
      end).

%% Exported: neighbour_distance_in_meters

neighbour_distance_in_meters() ->
    25.

%% Exported: degrees_to_meters

degrees_to_meters(Degrees) ->
    simulator:degrees_to_meters(Degrees, get_area()).

%% Exported: meters_to_degrees

meters_to_degrees(Meters) ->
    simulator:meters_to_degrees(Meters, get_area()).

%% Exported: width_height_in_meters

width_height_in_meters() ->
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude} = get_area(),
    simulator:width_height_in_meters(MinLongitude, MaxLongitude,
                                     MinLatitude, MaxLatitude).
