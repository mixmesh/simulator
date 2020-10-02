-module(simulator_config).
-export([make/0, make/2]).

-include_lib("apptools/include/shorthand.hrl").

make() ->
    make(<<"/tmp">>, 2).

make(RootDir, N) ->
    PlayersDir = filename:join([RootDir, <<"obscrete">>, <<"players">>]),
    Players = create_players(PlayersDir, N),
    io:format("~s\n", [jsone:encode(Players, [{indent, 2}, {space, 1}])]).

create_players(_PlayersDir, 0) ->
    [];
create_players(PlayersDir, N) ->
    PlayerDir = filename:join([PlayersDir, "p" ++ ?i2l(N)]),
    [[{<<"username">>, ?l2b("p" ++ ?i2l(N))},
      {<<"password">>, <<"baz">>},
      {<<"sync-address">>,
       ?l2b("127.0.0.1:" ++ ?i2l(10000 - N))},
      {<<"spiridon">>,
       [{<<"f">>, 0.2},
        {<<"k">>, 10}]},
      {<<"maildrop">>,
       [{<<"spooler-dir">>,
         filename:join([PlayerDir, "maildrop", "spooler"])}]},
      {<<"smtp-proxy">>,
       [{<<"address">>,
         ?l2b("127.0.0.1:" ++ ?i2l(20000 - N))},
        {<<"temp-dir">>,
         filename:join([PlayerDir, "smtp", "temp"])}]},
      {<<"pop3-proxy">>,
       [{<<"address">>,
         ?l2b("127.0.0.1:" ++ ?i2l(30000 - N))}]}]|
     create_players(PlayersDir, N - 1)].
