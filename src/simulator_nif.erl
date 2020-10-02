-module(simulator_nif).
-export([add_players/1, initialize/5, update_players/1]).
-on_load(init/0).

%% Exported: init

init() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(simulator), ?MODULE), none).

%% Exported: add_players

add_players(_NewPlayers) ->
    exit(nif_library_not_loaded).

%% Exported: initialize

initialize(_MinLongitude, _MaxLongitude,
           _MinLatitude, _MaxLatitude,
           _NeighbourDistance) ->
    exit(nif_library_not_loaded).

%% Exported: update_players

update_players(_UpdatedPlayers) ->
    exit(nif_library_not_loaded).
