-module(render_serv).
-export([start_link/0, stop/0]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("player/include/player_serv.hrl").

-record(state,
        {parent :: pid(),
         player_cache :: ets:tid()}).

-define(UPDATE_TIME, 500).

%% Exported: start_link

start_link() ->
    SimulatorModule = config:lookup([simulator, 'data-set']),
    Area = SimulatorModule:get_area(),
    {MinX, MaxX, MinY, MaxY} = Area,
    MetersToDegrees = fun SimulatorModule:meters_to_degrees/1,
    NeighbourDistance =
        MetersToDegrees(
          SimulatorModule:neighbour_distance_in_meters()),
    ok = simulator:initialize(MinX, MaxX, MinY, MaxY, NeighbourDistance),
    ?spawn_server_opts(fun init/1,
                       fun ?MODULE:message_handler/1,
                       #serv_options{name = ?MODULE}).

%% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent) ->
    PlayerCache = ets:new(player_cache, [{keypos, #db_player.nym}]),
    self() ! update,
    ?daemon_log_tag_fmt(system, "Render server has been started", []),
    {ok, #state{parent = Parent, player_cache = PlayerCache}}.

message_handler(#state{parent = Parent, player_cache = PlayerCache}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        update ->
            {NewPlayers, UpdatedPlayers} =
                player_db:foldl(
                  fun(#db_player{
                         nym = Nym,
                         x = X,
                         y = Y,
			 count = Count,
                         buffer_size = BufferSize,
                         neighbours = Neighbours1,
                         is_zombie = IsZombie,
                         pick_mode = PickMode} = Player,
                      {NewPlayers, UpdatedPlayers} = Acc) ->
			  Neighbours = [N || {N,up,connect} <- Neighbours1],
                          case ets:lookup(PlayerCache, Nym) of
                              [] when IsZombie ->
                                  Acc;
                              [] ->
                                  true = ets:insert(PlayerCache, Player),
                                  {[{Nym, X, Y}|NewPlayers], UpdatedPlayers};
                              [Player] ->
                                  Acc;
                              [CachedPlayer] ->
                                  UpdatedValues =
				      [{x,X},{y,Y}] ++
                                      %% is_any_updated(
                                      %%   [x, y], [X, Y],
                                      %%   [CachedPlayer#db_player.x,
                                      %%   CachedPlayer#db_player.y]) ++
                                      is_updated(
                                        buffer_size, BufferSize,
                                        CachedPlayer#db_player.buffer_size) ++
                                      is_updated(
                                        count, Count,
                                        CachedPlayer#db_player.count) ++
                                      is_updated(
                                        neighbours, Neighbours,
                                        CachedPlayer#db_player.neighbours) ++
                                      is_updated(
                                        is_zombie, IsZombie,
                                        CachedPlayer#db_player.is_zombie) ++
                                      is_updated(
                                        pick_mode, PickMode,
                                        CachedPlayer#db_player.pick_mode),
                                  if
                                      IsZombie andalso
                                      not CachedPlayer#db_player.is_zombie ->
                                          true = ets:delete(PlayerCache, Nym);
                                      true ->
                                          true = ets:insert(PlayerCache, Player)
                                  end,
                                  if
                                      length(UpdatedValues) > 0 ->
                                          {NewPlayers,
                                           [{Nym, UpdatedValues}|
                                            UpdatedPlayers]};
                                      true ->
                                          {NewPlayers, UpdatedPlayers}
                                  end
                          end
                  end, {[], []}),
            ?dbg_log({new_players, NewPlayers}),
            ok = simulator:add_players(NewPlayers),
            ?dbg_log({updated_players, UpdatedPlayers}),
            ok = simulator:update_players(UpdatedPlayers),
            erlang:send_after(?UPDATE_TIME, self(), update),
            noreply;
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

is_any_updated(Tags, Values, CachedValues) ->
    is_any_updated(Tags, Values, CachedValues, []).

is_any_updated([], _Values, _CachedValues, UpdatedValues) ->
    case lists:keymember(updated, 1, UpdatedValues) of
        true ->
            lists:reverse(lists:map(fun({_, Tag, Value}) ->
                                            {Tag, Value}
                                    end, UpdatedValues));
        false ->
            []
    end;
is_any_updated([Tag|RemainingTags],
               [Value|RemainingValues],
               [Value|RemainingCachedValues],
               UpdatedValues) ->
    is_any_updated(RemainingTags, RemainingValues, RemainingCachedValues,
                   [{same, Tag, Value}|UpdatedValues]);
is_any_updated([Tag|RemainingTags],
               [Value|RemainingValues],
               [_|RemainingCachedValues],
               UpdatedValues) ->
    is_any_updated(RemainingTags, RemainingValues, RemainingCachedValues,
                   [{updated, Tag, Value}|UpdatedValues]).

is_updated(_Tag, cached_value, _CachedValue) ->
    [];
is_updated(Tag, Value, _CachedValue) ->
    [{Tag, Value}].
