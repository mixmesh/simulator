-module(simulator_mkconfig).
-export([start/1]).

-include_lib("apptools/include/shorthand.hrl").

-spec start([string()]) -> no_return().

start([SourceCertFilename, DataSet]) ->
    ObscreteDir = <<"/tmp/obscrete">>,
    PkiDataDir = filename:join([ObscreteDir, <<"pki">>, <<"data">>]),
    ok = mkconfig:ensure_libs([PkiDataDir], true),
    PlayersDir = filename:join([ObscreteDir, "players"]),
    create_players(SourceCertFilename, PlayersDir,
                   get_location_index(DataSet)),
    mkconfig:return(0).

get_location_index("square") ->
    square:get_location_index();
get_location_index("circle") ->
    dummy_circle:get_location_index();
get_location_index("epfl") ->
    epfl_mobility:get_location_index();
get_location_index("roma") ->
    roma_taxi:get_location_index();
get_location_index("it") ->
    it_vr2marketbaiaotrial:get_location_index().

create_players(_SourceCertFilename, _PlayersDir, []) ->
    ok;
create_players(SourceCertFilename, PlayersDir, [{Name, _}|Rest]) ->
    PlayerDir = filename:join([PlayersDir, Name, <<"player">>]),
    PlayerTempDir = filename:join([PlayerDir, "temp"]),
    PlayerBufferDir = filename:join([PlayerDir, "buffer"]),
    PlayerPkiDataDir = filename:join([PlayerDir, "pki", "data"]),
    PlayerMaildropSpoolerDir =
        filename:join([PlayerDir, "maildrop", "spooler"]),
    PlayerSSLDir = filename:join([PlayerDir, "ssl"]),
    mkconfig:ensure_libs(
      [PlayerTempDir,
       PlayerBufferDir,
       PlayerPkiDataDir,
       PlayerMaildropSpoolerDir,
       PlayerSSLDir], true),
    TargetCertFilename = filename:join([PlayerDir, "ssl", "cert.pem"]),
    io:format("Copies ~s to ~s\n", [SourceCertFilename, TargetCertFilename]),
    case file:copy(SourceCertFilename, TargetCertFilename) of
        {ok, _} ->
            create_players(SourceCertFilename, PlayersDir, Rest);
        {error, Reason} ->
            io:format(standard_error, "~s: ~s\n",
                      [SourceCertFilename, file:format_error(Reason)]),
            mkconfig:return(100)
    end.
