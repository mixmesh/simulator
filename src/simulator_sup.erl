-module(simulator_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% Exported: start_link

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Exported: init

init([]) ->
    Render = config:lookup([simulator, renderer]),
    SimulatorPlayersSupSpec =
        #{id => simulator_players_sup,
          start => {simulator_players_sup, start_link, []},
          type => supervisor},
    SimulatorServSpec =
        #{id => simulator_serv,
          start => {simulator_serv, start_link, []}},
    NeighbourServSpec =
        #{id => neighbour_serv,
          start => {neighbour_serv, start_link, []}},
    RenderServSpec =
        #{id => render_serv,
          start => {Render, start_link, []}},
    {ok, {#{strategy => one_for_one}, [SimulatorPlayersSupSpec,
                                       SimulatorServSpec,
                                       NeighbourServSpec,
                                       RenderServSpec]}}.
