-module(square).
-export([get_area/0]).
-export([get_location_generator/1, get_location_index/0]).
-export([neighbour_distance_in_meters/0]).
-export([degrees_to_meters/1, meters_to_degrees/1]).
-export([width_height_in_meters/0]).

-include_lib("apptools/include/shorthand.hrl").

%% http://jsfiddle.net/geary/6cCvG/2/

%% A ten kilometer square in Silicon Valley
-define(NE10KM, {37.41504612935415, -121.96489836466651}).
-define(SW10KM, {37.32526847158805, -122.07799999999997}).

%% A one kilometer square in Silicon Valley
-define(NE1KM, {37.41509946129324, -122.06668983109768}).
-define(SW1KM, {37.406116847158806, -122.07799999999997}).

-define(PADDING, 0.5).

%% Exported: get_area

get_area() ->
    {MaxLatitude, MaxLongitude} = ?NE1KM,
    {MinLatitude, MinLongitude} = ?SW1KM,
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude}.

%% Exported: get_location_generator

get_location_generator(
  {Angle, DeltaAngle, RandomizeDeltaAngle, Timestamp, DeltaTimestamp,
   RandomizeDeltaTimestamp, Pi2, Radius, {X, Y} = Origo}) ->
    fun() ->
            UpdatedTimestamp =
                if RandomizeDeltaTimestamp ->
                        Timestamp + DeltaTimestamp * (rand:uniform() / 2 + 0.5);
                   true ->
                        Timestamp + DeltaTimestamp
                end,
            UpdatedAngle =
                if RandomizeDeltaAngle ->
                        Angle + DeltaAngle * (rand:uniform() / 2 + 0.5);
                   true ->
                        Angle + DeltaAngle
                end,
            CappedAngle =
                if abs(UpdatedAngle) > Pi2 ->
                        0;
                   true ->
                        UpdatedAngle
                end,
            Longitude = math:cos(CappedAngle) * Radius + X,
            Latitude = math:sin(CappedAngle) * Radius + Y,
            {{UpdatedTimestamp, Longitude, Latitude},
             get_location_generator(
               {CappedAngle, DeltaAngle, RandomizeDeltaAngle, UpdatedTimestamp,
                DeltaTimestamp, RandomizeDeltaTimestamp, Pi2, Radius, Origo})}
    end.

%% Exported: get_location_index
%% return players sorted by name {p1,1,_}...{pn,n,_}

get_location_index() ->
    N = case os:getenv("NPLAYER") of
	    false -> 100;
	    NStr -> list_to_integer(NStr)
	end,
    Pi2 = math:pi() * 2,
    %%get_location_index(100, Pi2 / 512, true, 1, true) %% ~10km/h
    get_location_index(1, N, Pi2 / 128, true, 1, true). %% ~50km/h

get_location_index(I, N,
                   DeltaAngle, RandomizeDeltaAngle,
                   DeltaTimestamp, RandomizeDeltaTimestamp) ->
    Pi2 = math:pi() * 2,
    Area = get_area(),
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude} = Area,
    Width = abs(MaxLongitude - MinLongitude),
    Height = abs(MaxLatitude - MinLatitude),
    get_location_index(I, N,
                       DeltaAngle, RandomizeDeltaAngle,
                       DeltaTimestamp, RandomizeDeltaTimestamp,
                       Pi2, Area, Width, Height).

get_location_index(I, N,
		   _DeltaAngle, _RandomizeDeltaAngle,
		   _DeltaTimestamp, _RandomizeDeltaTimestamp,
		   _Pi2,
		   _Area,
		   _Width, _Height) when I > N ->
    [];
get_location_index(I, N,
		   DeltaAngle, RandomizeDeltaAngle,
		   DeltaTimestamp, RandomizeDeltaTimestamp,
		   Pi2,
		   {MinLongitude, _MaxLongitude, MinLatitude, _MaxLatitude} =
		       Area,
		   Width, Height) ->
    Label = ?l2b([<<"p">>, ?i2b(I)]),
    PaddedWidth = Width * ?PADDING,
    PaddedHeight = Height * ?PADDING,
    X = rand:uniform() * PaddedWidth,
    Y = rand:uniform() * PaddedHeight,
    Longitude = MinLongitude + ((Width - PaddedWidth) / 2) + X,
    Latitude = MinLatitude + ((Height - PaddedHeight) / 2) + Y,
    Radius = max_radius(Area, Longitude, Latitude),
    StartAngle = Pi2 * rand:uniform(),
    {UpdatedStartAngle, UpdatedDeltaAngle} =
        case rand:uniform(2) == 2 of
            true ->
                {StartAngle, DeltaAngle};
            false ->
                {-1 * StartAngle, -1 * DeltaAngle}
        end,
    [{Label,I,
      {UpdatedStartAngle, UpdatedDeltaAngle, RandomizeDeltaAngle, 0,
       DeltaTimestamp, RandomizeDeltaTimestamp, Pi2, Radius,
       {Longitude, Latitude}}} |
     get_location_index(I+1,N,
                        UpdatedDeltaAngle, RandomizeDeltaAngle,
                        DeltaTimestamp, RandomizeDeltaTimestamp,
                        Pi2,
                        Area,
                        Width, Height)].

max_radius({MinLongitude, MaxLongitude, MinLatitude, MaxLatitude},
           Longitude, Latitude) ->
    min(min(Longitude - MinLongitude, MaxLongitude - Longitude),
        min(Latitude - MinLatitude, MaxLatitude - Latitude)) *
        ((rand:uniform() / 2) + 0.5).

%% Exported: neighbour_distance_in_meters

neighbour_distance_in_meters() ->
    75.

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
