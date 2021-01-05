-ifndef(SIMULATOR_LOCATION_HRL).
-define(SIMULATOR_LOCATION_HRL, true).

-record(simulator_location,
        {geodetic_center :: {number(), number()},
         ecef_center :: {number(), number(), number()},
         zoom :: integer(),
         scale :: integer(),
         area :: {number(), number(), number(), number()},
         meters_per_pixel :: number(),
         width_in_pixels :: integer(),
         height_in_pixels :: integer(),
         width_in_meters :: number(),
         height_in_meters :: number(),
         width_in_degrees :: number(),
         height_in_degrees :: number(),
         degrees_per_meter :: number(),
         meters_per_degree :: number(),
         max_speed_in_meters_per_second :: number(),
         max_speed_in_degrees :: number(),
         update_frequency :: number(),
         degrees_per_update :: number(),
         neighbour_distance_in_degrees :: number(),
         neighbour_distance_in_meters :: number()}).

-endif.
