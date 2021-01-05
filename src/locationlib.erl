-module(locationlib).
-export([geodetic_to_ecef_coordinates/3,
         meters_per_pixel/2,
         distance/4,
         get_corners/4]).

-define(PI, 3.141592).
-define(RADIANS_PER_DEGREE, (?PI / 180)).
-define(MERCATOR_RANGE, 256).
-define(PIXEL_ORIGIN, {?MERCATOR_RANGE / 2, ?MERCATOR_RANGE / 2}).
-define(PIXELS_PER_LONGITUDE_DEGREE, (?MERCATOR_RANGE / 360)).
-define(PIXELS_PER_LONGITUDE_RADIAN, (?MERCATOR_RANGE / (2 * ?PI))).

%% Exported: geodetic_to_ecef_coordinates

%% https://en.m.wikipedia.org/wiki/Geographic_coordinate_conversion#From_geodetic_to_ECEF_coordinates 
%% https://en.m.wikipedia.org/wiki/Geodetic_datum#World_Geodetic_System_1984_(WGS_84)

geodetic_to_ecef_coordinates(Longitude, Latitude, Altitude) ->
    CLongitude = math:cos(Longitude * ?RADIANS_PER_DEGREE),
    SLongitude = math:sin(Longitude  * ?RADIANS_PER_DEGREE),
    CLatitude = math:cos(Latitude * ?RADIANS_PER_DEGREE),
    SLatitude = math:sin(Latitude *  ?RADIANS_PER_DEGREE),
    %% Semi-major axis
    A = 6378137.0,
    A2 = math:pow(A, 2),
    %% Semi-minor axis
    B = 6356752.3142,
    B2 = math:pow(B, 2),
    %% Prime vertical radius of curvature
    N = A2 / math:sqrt(
               math:pow(CLatitude, 2) * A2 + math:pow(SLatitude, 2) * B2),
    X = (N + Altitude) * CLatitude * CLongitude, 
    Y = (N + Altitude) * CLatitude * SLongitude, 
    Z  = (B2 / A2 * N + Altitude) * SLatitude,
    {X, Y, Z}.

%% Exported: meters_per_pixel

%% https://groups.google.com/g/google-maps-js-api-v3/c/hDRO4oHVSeM?pli=1

meters_per_pixel({_Longitude, Latitude}, Zoom) ->
    meters_per_pixel(Latitude, Zoom);
meters_per_pixel(Latitude, Zoom) ->
    156543.03392 * math:cos(Latitude * ?PI / 180) / math:pow(2, Zoom).

%% Exported: distance

%% Note: We should use https://en.wikipedia.org/wiki/Haversine_formula
distance(X1, Y1, X2, Y2) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% Exported: get_corners

%% https://developers.google.com/maps/documentation/javascript/coordinates
%% https://stackoverflow.com/questions/12507274/how-to-get-bounds-of-a-google-static-map
get_corners(GeodeticCenter, Zoom, MapWidth, MapHeight) ->
    Scale = math:pow(2, Zoom),
    {CenterX, CenterY} = from_longitude_latitude_to_pixel(GeodeticCenter),
    SWPixel =
        {CenterX - (MapWidth / 2) / Scale, CenterY + (MapHeight / 2) / Scale},
    SWLongitudeLatitude = from_pixel_to_longitude_latitude(SWPixel),
    NEPixel =
        {CenterX + (MapWidth / 2) / Scale, CenterY - (MapHeight / 2) / Scale},
    NELongitudeLatitude = from_pixel_to_longitude_latitude(NEPixel),
    {SWLongitudeLatitude, NELongitudeLatitude}.

from_longitude_latitude_to_pixel({Longitude, Latitude}) ->
    {OriginX, OriginY} = ?PIXEL_ORIGIN,
    PixelX = OriginX + Longitude * ?PIXELS_PER_LONGITUDE_DEGREE,
    %% NOTE: Truncating to 0.9999 effectively limits latitude to
    %% 89.189. This is about a third of a tile past the edge of the
    %% world tile.
    SinY = bound(math:sin(degrees_to_radians(Latitude)), -0.9999, 0.9999),
    PixelY =
        OriginY + 0.5 * math:log((1 + SinY) / (1 - SinY)) *
        -?PIXELS_PER_LONGITUDE_RADIAN,
    {PixelX, PixelY}.

bound(Value, Min, Max) ->
    min(max(Value, Min), Max).

degrees_to_radians(Degrees) ->
    Degrees * ?RADIANS_PER_DEGREE.

from_pixel_to_longitude_latitude({X, Y}) ->
    {OriginX, OriginY} = ?PIXEL_ORIGIN,
    Longitude = (X - OriginX) / ?PIXELS_PER_LONGITUDE_DEGREE,
    LatitudeRadians = (Y - OriginY) / -?PIXELS_PER_LONGITUDE_RADIAN,
    Latitude =
        radians_to_degrees(2 * math:atan(math:exp(LatitudeRadians)) - ?PI / 2),
    {Longitude, Latitude}.

radians_to_degrees(Radians) ->
    Radians / ?RADIANS_PER_DEGREE.
