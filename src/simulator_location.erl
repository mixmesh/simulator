-module(simulator_location).
-export([get/1]).

-include("simulator_location.hrl").

%% St Olofsgatan
%% https://maps.googleapis.com/maps/api/staticmap?center=59.865906,17.644667&zoom=16&size=500x500&scale=2&key=<YOUR-KEY>
get(stolofsgatan) ->
    BackgroundFilename =
        filename:join([code:priv_dir(simulator), "stolofsgatan.png"]),
    get(17.644667, 59.865906, 16, 500, 500, 2, 16, 1, 100, BackgroundFilename).

get(Longitude, Latitude, Zoom, WidthInPixels, HeightInPixels, Scale,
    MaxSpeedInMetersPerSecond, UpdateFrequency, NeighbourDistanceInMeters,
    BackgroundFilename) ->
    GeodeticCenter = {Longitude, Latitude},
    ECEFCenter =
        locationlib:geodetic_to_ecef_coordinates(Longitude, Latitude, 0),
    {{SWLongitude, SWLatitude}, {NELongitude, NELatitude}} = 
        locationlib:get_corners(
          {Longitude, Latitude}, Zoom, WidthInPixels, HeightInPixels),
    WidthInDegrees = NELongitude - SWLongitude,
    HeightInDegrees = NELatitude - SWLatitude,
    Area = {SWLongitude, NELongitude, SWLatitude, NELatitude},
    MetersPerPixel = locationlib:meters_per_pixel(GeodeticCenter, Zoom),
    WidthInMeters = MetersPerPixel * WidthInPixels,
    HeightInMeters = MetersPerPixel * HeightInPixels,
    AverageDegreesPerMeter =
        ((NELongitude - SWLongitude) / WidthInMeters +
             (NELatitude - SWLatitude) / HeightInMeters) / 2,
    AverageMetersPerDegree =
        (WidthInMeters / (NELongitude - SWLongitude) +
             HeightInMeters / (NELatitude - SWLatitude)) / 2,
    MaxSpeedInDegrees = MaxSpeedInMetersPerSecond * AverageDegreesPerMeter,
    DegreesPerUpdate = MaxSpeedInDegrees * UpdateFrequency,
    NeighbourDistanceInDegrees =
        NeighbourDistanceInMeters * AverageDegreesPerMeter,
    #simulator_location{
       geodetic_center = GeodeticCenter,
       ecef_center = ECEFCenter,
       zoom = Zoom,
       scale = Scale,
       area = Area,
       meters_per_pixel = MetersPerPixel,
       width_in_pixels = WidthInPixels,
       height_in_pixels = HeightInPixels,
       width_in_meters = WidthInMeters,
       height_in_meters = HeightInMeters,
       width_in_degrees = WidthInDegrees,
       height_in_degrees = HeightInDegrees,
       degrees_per_meter = AverageDegreesPerMeter,
       meters_per_degree = AverageMetersPerDegree,
       max_speed_in_meters_per_second = MaxSpeedInMetersPerSecond,
       max_speed_in_degrees = MaxSpeedInDegrees,
       update_frequency = UpdateFrequency,
       degrees_per_update = DegreesPerUpdate,
       neighbour_distance_in_meters = NeighbourDistanceInMeters,
       neighbour_distance_in_degrees = NeighbourDistanceInDegrees,
       background_filename = BackgroundFilename}.
