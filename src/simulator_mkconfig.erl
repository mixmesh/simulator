-module(simulator_mkconfig).
-export([start/1]).

-include_lib("apptools/include/shorthand.hrl").

%% Exported: start

-spec start([string()]) -> no_return().

start([SourceCertFilename, DataSet]) ->
    ObscreteDir = <<"/tmp/obscrete">>,
    GlobalPkiDir = filename:join([ObscreteDir, <<"global-pki">>]),
    true = mkconfig:ensure_libs(command, [GlobalPkiDir], true),
    PlayersDir = filename:join([ObscreteDir, <<"players">>]),
    ok = create_players(SourceCertFilename, PlayersDir,
                        get_location_index(DataSet)),
    mkconfig:return(command, 0).

get_location_index("square") ->
    square:get_location_index();
get_location_index("circle") ->
    dummy_circle:get_location_index();
get_location_index("epfl") ->
    epfl_mobility:get_location_index();
get_location_index("roma") ->
    roma_taxi:get_location_index();
get_location_index("it") ->
    it_vr2marketbaiaotrial:get_location_index();
get_location_index("mesh") ->
    mesh:get_location_index().

create_players(_SourceCertFilename, _PlayersDir, []) ->
    ok;
create_players(SourceCertFilename, PlayersDir, [{Nym, _}|Rest]) ->
    mkconfig:create_player(PlayersDir, SourceCertFilename, ?b2l(Nym), command),
    create_players(SourceCertFilename, PlayersDir, Rest).
