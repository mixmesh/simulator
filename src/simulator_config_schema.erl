-module(simulator_config_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").

get() ->
    [{simulator,
      [{enabled,
        #json_type{
           name = bool,
           typical = false,
           reloadable = false}},
       {'data-set',
        #json_type{
           name = atom,
           info = <<"One of circle, square, epfl, roma or it">>,
           typical = square,
           convert = fun(square) -> square;
                        (epfl) -> epfl_mobility;
                        (circle) -> dummy_circle;
                        (roma) -> roma_taxi;
                        (it) -> it_vr2marketbaiaotrial;
                        (_) -> throw({failed, <<"Must be on of \"circle\", \"square\", \"epfl\", \"roma\" or \"it\"">>})
                     end,
           reloadable = false}}]}].
