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
           info = "square, mesh or random-walk",
           typical = square,
           transform =
               fun(square) -> square;
		  (mesh) -> mesh;
                  ('random-walk') -> random_walk;
                  (_) ->
                       throw(
                         {failed,
                          "Must be one of square, mesh or random-walk"})
               end,
           untransform =
               fun(random_walk) -> 'random-walk';
                  (Value) -> Value
               end,
           reloadable = false}}]}].
