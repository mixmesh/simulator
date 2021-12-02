-module(neighbour_serv).
-export([start_link/0, stop/0]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("player/include/player_serv.hrl").
-include_lib("rstar/include/rstar.hrl").

-define(UPDATE_TIME, 500).

-record(state,
        {parent :: pid(),
         players :: [{#player{}, [#player{}]}],
         neighbour_distance :: number()}).

%%
%% Exported: start_link
%%

start_link() ->
    ?spawn_server(fun init/1,
                  fun ?MODULE:message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent) ->
    Players = lists:map(fun(Player) ->
                                {Player, []}
                        end, simulator_serv:get_players()),
    SimulatorModule = config:lookup([simulator, 'data-set']),
    NeighbourDistance = SimulatorModule:neighbour_distance(),
    self() ! update,
    ?daemon_log_tag_fmt(system, "Neighbour server has been started", []),
    {ok, #state{parent = Parent,
                players = Players,
                neighbour_distance = NeighbourDistance}}.

message_handler(#state{parent = Parent,
                       players = Players,
                       neighbour_distance = NeighbourDistance} = State) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        update ->
            UpdatedPlayers = update_players(Players, NeighbourDistance),
            erlang:send_after(?UPDATE_TIME, self(), update),
            {noreply, State#state{players = UpdatedPlayers}};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

%%
%% Update players
%%

update_players(Players, NeighbourDistance) ->
    Tree = build_rstar_tree(rstar:new(2), Players),
    inform_players(Tree, Players, NeighbourDistance).

build_rstar_tree(Tree, []) ->
    Tree;
build_rstar_tree(Tree, [{#player{nym = Nym} = Player, _Neighbours}|Rest]) ->
    case player_db:lookup(Nym) of
        [] ->
            build_rstar_tree(Tree, Rest);
        [#db_player{is_zombie = true}] ->
            build_rstar_tree(Tree, Rest);
        [#db_player{x = X, y = Y}] ->
            Point = rstar_geometry:point2d(X, Y, Player),
            NewTree = rstar:insert(Tree, Point),
            build_rstar_tree(NewTree, Rest)
    end.

inform_players(_Tree, [], _NeighbourDistance) ->
    [];
inform_players(Tree,
               [{#player{nym = Nym, nodis_serv_pid = NodisServPid} = Player,
                 _OldNeighbours}|Rest],
               NeighbourDistance) ->
    NewNeighbours = get_neighbours(Tree, NeighbourDistance, Nym),
    lists:foreach(
      fun(#player{nym = NNym, sync_address = {SyncIpAddress, SyncPort}}) ->
	      OutPort = SyncPort + 1, %% simulate outport
	      [#db_player{ x=Longitude,
			   y=Latitude }] = player_db:lookup(NNym),
              ok = nodis_serv:simping(
                     NodisServPid, SyncIpAddress, OutPort,
		     #{ ping_interval => ?UPDATE_TIME,
			lat => Latitude, long => Longitude })
      end, NewNeighbours),
    [{Player, NewNeighbours}|inform_players(Tree, Rest, NeighbourDistance)].

get_neighbours(Tree, NeighbourDistance, Nym) ->
    case player_db:lookup(Nym) of
        [] ->
            [];
        [#db_player{x = X, y = Y}] ->
            Point = rstar_geometry:point2d(X, Y, undefined),
            Matches = rstar:search_around(Tree, Point, NeighbourDistance),
            lists:sort(
              lists:foldl(
                fun(#geometry{value = #player{nym = PlayerNym}}, Acc)
                      when PlayerNym == Nym ->
                        Acc;
                   (#geometry{value = Player}, Acc) ->
                        [Player|Acc]
                end, [], Matches))
    end.
