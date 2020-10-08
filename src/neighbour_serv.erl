-module(neighbour_serv).
-export([start_link/0, stop/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("player/include/player_serv.hrl").
-include_lib("simulator/include/player_db.hrl").
-include_lib("rstar/include/rstar.hrl").
-include("neighbour_serv.hrl").

-define(UPDATE_TIME, 500).

-record(state,
        {parent             :: pid(),
         players            :: [{#player{}, [#player{}]}],
         neighbour_distance :: number()}).

%% Exported: start_link

start_link() ->
    ?spawn_server_opts(fun init/1,
                       fun message_handler/1,
                       #serv_options{name = ?MODULE}).

%% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent) ->
    Players = lists:map(fun(Player) ->
                                {Player, []}
                        end, simulator_serv:get_players()),
    NeighbourDistance =
        simulator_serv:meters_to_degrees(?NEIGHBOUR_DISTANCE_IN_METERS),
    %%?dbg_log({neighbour_distance, NeighbourDistance}),
    self() ! update,
    ?daemon_tag_log(system, "Neighbour server has been started", []),
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
build_rstar_tree(Tree, [{#player{name = Name} = Player, _Neighbours}|Rest]) ->
    case player_db:lookup(Name) of
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
               [{#player{name = Name,
                         player_serv_pid = PlayerServPid,
                         nodis_serv_pid = NodisServPid} = Player,
                 OldNeighbours}|Rest],
               NeighbourDistance) ->
    case get_neighbours(Tree, NeighbourDistance, Name) of
        OldNeighbours ->
            [{Player, OldNeighbours}|
             inform_players(Tree, Rest, NeighbourDistance)];
        NewNeighbours ->
            lists:foreach(
              fun(#player{sync_ip_address = SyncIpAddress,
                          sync_port = SyncPort}) ->
                      ok = nodis_srv:simping(
                             NodisServPid, SyncIpAddress, SyncPort,
                             ?UPDATE_TIME)
              end, NewNeighbours),
            [{Player, NewNeighbours}|
             inform_players(Tree, Rest, NeighbourDistance)]
    end.

get_neighbours(Tree, NeighbourDistance, Name) ->
    case player_db:lookup(Name) of
        [] ->
            [];
        [#db_player{x = X, y = Y}] ->
            Point = rstar_geometry:point2d(X, Y, undefined),
            Matches = rstar:search_around(Tree, Point, NeighbourDistance),
            lists:sort(
              lists:foldl(
                fun(#geometry{value = #player{name = PlayerName}}, Acc)
                      when PlayerName == Name->
                        Acc;
                   (#geometry{value = Player}, Acc) ->
                        [Player|Acc]
                end, [], Matches))
    end.
