-module(simulator_pki_serv).
-export([start_link/0, stop/0]).
-export([generate_keys/0, get_keys/1]).
-export([get_player/1, get_random_player/1]).
-export([set_players/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("elgamal/include/elgamal.hrl").

-record(state,
        {parent :: pid(),
         db     :: ets:tid()}).

-record(player,
        {n    :: integer() | '_',
         name :: binary(),
         keys :: {#pk{}, #sk{}} | '_' | '$1'}).

%% Exported: start_link

start_link() ->
    ?spawn_server_opts(fun init/1,
                       fun message_handler/1,
                       #serv_options{name = ?MODULE}).

% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%% Exported: generate_keys

generate_keys() ->
    elgamal:generate_key_pair().

%% Exported: get_keys

get_keys(Name) ->
    serv:call(?MODULE, {get_keys, Name}).

%% Exported: get_player

get_player(Name) ->
    serv:call(?MODULE, {get_player, Name}).

%% Exported: get_random_player

get_random_player(Name) ->
    serv:call(?MODULE, {get_random_player, Name}).

%% Exported: set_players

set_players(Names) ->
    serv:call(?MODULE, {set_players, Names}).

%%
%% Server
%%

init(Parent) ->
    rand:seed(exsss),
    Db = ets:new(db, [ordered_set, {keypos, #player.n}]),
    ?daemon_tag_log(system, "PKI server has been started", []),
    {ok, #state{parent = Parent, db = Db}}.

message_handler(#state{parent = Parent, db = Db}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {call, From, {get_keys, Name}} ->
            case ets:match(Db, #player{n = '_', name = Name, keys = '$1'}) of
                [[Keys]] ->
                    {reply, From, {ok, Keys}};
                _ ->
                    {reply, From, {error, unknown_player}}
            end;
        {call, From, {get_player, Name}} ->
            case ets:match_object(Db, #player{n = '_', name = Name, keys = '_'}) of
                [#player{keys = {PublicKey, _}}] ->
                    {reply, From, {ok, {Name, PublicKey}}};
                _ ->
                    {reply, From, {error, unknown_player}}
            end;
        {call, From, {get_random_player, Name}} ->
            Size = ets:info(Db, size),
            K = rand:uniform(Size) - 1,
            case ets:lookup(Db, K) of
                [#player{name = Name}] ->
                    N =
                        if
                            K == Size - 1 ->
                                K - 1;
                            true ->
                                K + 1
                        end,
                    [#player{name = Name,
                             keys = {PublicKey, _}}] = ets:lookup(Db, N),
                    {reply, From, {Name, PublicKey}};
                [#player{name = Name, keys = {PublicKey, _}}] ->
                    {reply, From, {Name, PublicKey}}
            end;
        {call, From, {set_players, Names}} ->
            lists:foldl(
              fun(Name, N) ->
                      Keys = elgamal:generate_key_pair(),
                      true = ets:insert(Db, #player{n = N,
                                                    name = Name,
                                                    keys = Keys}),
                      N + 1
              end, 0, Names),
            {reply, From, ok};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.
