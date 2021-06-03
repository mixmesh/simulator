-module(simulator_players_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%
%% Exported: start_link
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Exported: init
%%

init([]) ->
    PlayerSupSpec = #{id => player_sup,
                      start => {player_sup, start_link, []},
                      type => supervisor},
    {ok, {#{strategy => simple_one_for_one}, [PlayerSupSpec]}}.
