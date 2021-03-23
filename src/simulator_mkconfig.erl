-module(simulator_mkconfig).
-export([start/1]).

-include_lib("apptools/include/shorthand.hrl").

%% Exported: start

-spec start([string()]) -> no_return().

start([SourceCertFilename, DataSet]) ->
    MixmeshDir = <<"/tmp/mixmesh">>,
    RemoteKeydirDir = filename:join([MixmeshDir, <<"remote-keydir">>]),
    SSLDir = filename:join([RemoteKeydirDir, <<"ssl">>]),
    try
        true = mkconfig:ensure_libs(stdout, [SSLDir], true),
        true = mkconfig:copy_certificate(stdout, SourceCertFilename, SSLDir),
        PlayersDir = filename:join([MixmeshDir, <<"players">>]),
        ok = create_players(SourceCertFilename, PlayersDir,
                            get_location_index(DataSet)),
        PinFilename = filename:join([MixmeshDir, <<"pin">>]),
        io:format("Creates dummy ~s\n", [PinFilename]),
        ok = file:write_file(PinFilename, <<"123456">>),
        erlang:halt(0)
    catch
        throw:{status, Status} ->
            erlang:halt(Status)
    end.

get_location_index("square") ->
    square:get_location_index();
get_location_index("mesh") ->
    mesh:get_location_index();
get_location_index("random_walk") ->
    random_walk:get_location_index().

create_players(_SourceCertFilename, _PlayersDir, []) ->
    ok;
create_players(SourceCertFilename, PlayersDir, [{Nym,_,_}|Rest]) ->
    mkconfig:create_player(
      stdout, PlayersDir, SourceCertFilename, ?b2l(Nym)),
    create_players(SourceCertFilename, PlayersDir, Rest).
