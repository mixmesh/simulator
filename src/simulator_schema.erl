-module(simulator_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").

get() ->
    [{simulator,
      [{enabled,
        #json_type{
           name = bool,
           typical = false,
           reloadable = false}},
       {'renderer',
        #json_type{
           name = atom,
           info = "sdl or epx",
           typical = sdl,
           transform =
               fun(sdl) -> render_serv;
                  (epx) -> render_epx;
                  (_) ->
                       throw({failed, "Must be one of sdl or epx"})
               end,
           untransform =
               fun(render_serv) -> sdl;
                  (render_epx) -> epx
               end,
           reloadable = false}},
       {'data-set',
        #json_type{
           name = atom,
           info = "circle, square, epfl, roma, mesh or it",
           typical = square,
           transform =
               fun(square) -> square;
                  (epfl) -> epfl_mobility;
                  (circle) -> dummy_circle;
                  (roma) -> roma_taxi;
                  (it) -> it_vr2marketbaiaotrial;
		  (mesh) -> mesh;
                  (_) ->
                       throw(
                         {failed,
                          "Must be one of circle, square, epfl, "
			  "roma, it or mesh"})
               end,
           untransform =
               fun(epfl_mobility) -> epfl;
                  (dummy_circle) -> circle;
                  (roma_taxi) -> roma;
                  (it_vr2marketbaiaotrial) -> it;
                  (Value) -> Value
               end,
           reloadable = false}}]}].
