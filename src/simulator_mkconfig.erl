-module(simulator_mkconfig).
-export([start/1]).

-include_lib("apptools/include/shorthand.hrl").

-spec start([string()]) -> no_return().

start([DataSet]) ->
    ObscreteDir = <<"/tmp/obscrete">>,
    PkiDataDir = filename:join([ObscreteDir, <<"pki">>, <<"data">>]),
    ok = mkconfig:ensure_libs([PkiDataDir], false),
    PlayersDir = filename:join([ObscreteDir, "players"]),
    create_players(PlayersDir, get_location_index(DataSet)),
    mkconfig:return(0).

get_location_index("square") ->
    square:get_location_index();
get_location_index("circle") ->
    dummy_circle:get_location_index();
get_location_index("epfl") ->
    epfl_mobility:get_location_index();
get_location_index("it") ->
    it_vr2marketbaiaotrial:get_location_index().

create_players(_PlayersDir, []) ->
    ok;
create_players(PlayersDir, [{Name, _}|Rest]) ->
    PlayerDir = filename:join([PlayersDir, Name, <<"player">>]),
    PlayerTempDir = filename:join([PlayerDir, "temp"]),
    PlayerPkiDataDir = filename:join([PlayerDir, "pki", "data"]),
    PlayerMaildropSpoolerDir =
        filename:join([PlayerDir, "maildrop", "spooler"]),
    mkconfig:ensure_libs(
      [PlayerTempDir, PlayerPkiDataDir, PlayerMaildropSpoolerDir], true),
    create_players(PlayersDir, Rest).