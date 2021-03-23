-module(simulator_serv).
-export([start_link/0, stop/0]).
-export([elect_source_and_target/3]).
-export([get_players/0]).
-export([get_player_nyms/1]).
-export([get_random_player/1]).
-export([target_received_message/2]).
-export([received_message/2]).
-export([analyze/0]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("player/include/player_serv.hrl").
-include("simulator_location.hrl").

-define(SIMULATION_TIME, (1000 * 60 * 60 * 10)).
-define(ANALYZE_TIME, (1000 * 30)).

-define(SYNC_IP_ADDRESS, {127, 0, 0, 1}).
-define(SYNC_BASE, 4000).

-define(SMTP_IP_ADDRESS, {127, 0, 0, 1}).
-define(SMTP_BASE, 16000).

-define(POP3_IP_ADDRESS, {127, 0, 0, 1}).
-define(POP3_BASE, 32000).

-define(HTTP_IP_ADDRESS, {127, 0, 0, 1}).
-define(HTTP_BASE, 48000).

-define(KEYDIR_IP_ADDRESS, {127, 0, 0, 1}).
-define(KEYDIR_PORT, 11112).

-define(BUFFER_SIZE, 1000).
-define(F, 0.2).
-define(K, 10).

-record(state,
        {parent :: pid(),
         source = none :: none | binary(),
         target = none :: none | binary(),
         players :: [#player{}],
         k :: integer()}).

%% Exported: start_link

start_link() ->
    ?spawn_server_opts(fun init/1,
                       fun ?MODULE:message_handler/1,
                       #serv_options{name = ?MODULE}).

%% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%% Exported: elect_source_and_target

elect_source_and_target(MessageMD5, SourceNym, TargetNym) ->
    serv:cast(?MODULE,
              {elect_source_and_target, MessageMD5,  SourceNym, TargetNym}).

%% Exported: get_players

get_players() ->
    serv:call(?MODULE, get_players).

%% Exported: get_player_nyms

get_player_nyms(SyncAddresses) ->
    serv:call(?MODULE, {get_player_nyms, SyncAddresses}).

%% Exported: get_random_player

get_random_player(Nym) ->
    serv:call(?MODULE, {get_random_player, Nym}).

%% Exported: target_received_message

target_received_message(TargetNym, SourceNym) ->
    serv:cast(?MODULE, {target_received_message, TargetNym, SourceNym}).

%% Exported: received_message

received_message(TargetNym, SourceNym) ->
    serv:cast(?MODULE, {received_message, TargetNym, SourceNym}).

%% Exported: analyze

analyze() ->
    serv:cast(?MODULE, analyze).

%%
%% Server
%%

init(Parent) ->
    rand:seed(exsss),
    %% Keeps track on messages on simulated nodes (like a global server..)
    ets:new(player_message, [public, named_table]),
    %%io:format("player_message table created ~p\n", [ets:info(player_message)]),
    player_info:new(),
    SimulatorModule = config:lookup([simulator, 'data-set']),
    Location = SimulatorModule:get_location(),
    true = player_db:new(),
    true = stats_db:new(),
    %% FIXME: find a place
    %% ets:new(endpoint_reg,[public, named_table, {write_concurrency, true}]),
    
    %% Start simulated players
    LocationIndex = SimulatorModule:get_location_index(),
    AllPlayers =
        lists:foldl(
          fun({Nym, I, Opaque}, Players) ->
                  PlayersDir = <<"/tmp/mixmesh/players">>,
                  Keys = elgamal:generate_key_pair(
                           Nym, binary:decode_unsigned(Nym)),
                  GetLocationGenerator =
                      fun() ->
                              SimulatorModule:get_location_generator(Opaque)
                      end,
                  %% baz
                  SmtpPasswordDigest =
                      <<237,85,139,97,91,27,175,166,8,177,220,107,101,160,138, 249,172,253,25,226,211,31,248,2,107,122,138,12,220,97,183,183,182,89,251,10,55,198,134,85,162,164,229,128,66, 117,157,133,43,78,200,39,225,175,154,203,77,253,243,200, 31,87,99,156>>,
                  %% baz
                  Pop3PasswordDigest =
                      <<237,85,139,97,91,27,175,166,8,177,220,107,101,160,138, 249,172,253,25,226,211,31,248,2,107,122,138,12,220,97,183,183,182,89,251,10,55,198,134,85,162,164,229,128,66, 117,157,133,43,78,200,39,225,175,154,203,77,253,243,200, 31,87,99,156>>,
                  HttpPassword = <<"hello">>,
                  %%KeydirMode = {remote, <<"baz">>,
                  %%           {tcp_only, {?KEYDIR_IP_ADDRESS, ?KEYDIR_PORT}}},
                  KeydirMode = local,
		  SmtpPort = ?SMTP_BASE+I,
		  Pop3Port = ?POP3_BASE+I,
		  SyncPort = ?SYNC_BASE+2*I,
		  HttpPort = ?HTTP_BASE+I,
                  {ok, PlayerSupPid} =
                      supervisor:start_child(
                        simulator_players_sup,
                        [#simulated_player_serv_config{
                            players_dir = PlayersDir,
                            nym = Nym,
                            routing_type = location,
                            use_gps = true,
                            longitude = 0.0,
                            latitude = 0.0,
                            sync_address = {?SYNC_IP_ADDRESS, SyncPort},
                            buffer_size = ?BUFFER_SIZE,
                            f = ?F,
                            k = ?K,
                            keys = Keys,
                            get_location_generator = GetLocationGenerator,
                            smtp_address = {?SMTP_IP_ADDRESS, SmtpPort},
                            smtp_password_digest = SmtpPasswordDigest,
                            pop3_address = {?POP3_IP_ADDRESS, Pop3Port},
                            pop3_password_digest = Pop3PasswordDigest,
                            http_address = [{?HTTP_IP_ADDRESS, HttpPort}],
                            http_password = HttpPassword,
                            keydir_mode = KeydirMode}]),
                  {ok, PlayerServPid} =
                      get_child_pid(PlayerSupPid, player_serv),
                  {ok, NodisServPid} =
                      get_child_pid(PlayerSupPid, nodis_serv),
		  player_info:set(Nym, smtp_port, SmtpPort),
		  player_info:set(Nym, pop3_port, Pop3Port),
		  player_info:set(Nym, sync_port, SyncPort),
		  player_info:set(Nym, http_port, HttpPort),
		  player_info:set(Nym, player_serv, PlayerServPid),
		  player_info:set(Nym, nodis_serv, NodisServPid),
		  [#player{nym = Nym,
			   player_serv_pid = PlayerServPid,
			   nodis_serv_pid = NodisServPid,
			   sync_address = {?SYNC_IP_ADDRESS, SyncPort},
			   smtp_address = {?SMTP_IP_ADDRESS, SmtpPort}} |
		   Players]
          end, [], LocationIndex),
    %% Start player location updating
    lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                          player_serv:start_location_updating(PlayerServPid)
                  end, AllPlayers),
    %% Should we put a player in the center of the area?    
    case SimulatorModule:center_target() of
        {true, Nym} ->
            {found, #player{player_serv_pid = PlayerServPid}} =
                get_player(Nym, AllPlayers),
            spawn(fun() ->
                          timer:sleep(5000),
                          ok = player_serv:pin_location(
                                 PlayerServPid,
                                 Location#simulator_location.geodetic_center),
                          SimulatorModule:send_simulated_messages(AllPlayers)
                  end);
        false ->
            SimulatorModule:send_simulated_messages(AllPlayers)
    end,
    erlang:send_after(?SIMULATION_TIME, self(), simulation_ended),
    timer:send_interval(?ANALYZE_TIME, analyze),
    K = config:lookup([player, 'sync-server', k]),
    ?daemon_log_tag_fmt(system, "Master server has been started", []),
    {ok, #state{parent = Parent, players = AllPlayers, k = K}}.

get_child_pid(SupervisorPid, ChildId) ->
    {value, {_Id, ChildPid, _Type, _Modules}} =
        lists:keysearch(ChildId, 1, supervisor:which_children(SupervisorPid)),
    {ok, ChildPid}.

message_handler(#state{parent = Parent, source = Source, target = Target,
                       players = Players, k = K} = State) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {cast, {elect_source_and_target, MessageMD5, SourceNym, TargetNym}}
          when Source == none andalso Target == none ->
            ok = elect_players(Players, MessageMD5, SourceNym, TargetNym),
            {noreply, State#state{source = SourceNym, target = TargetNym}};
        {cast, {elect_source_and_target, _MessageMD5, _SourceNym, _TargetNym}} ->
            noreply;
        {call, From, get_players} ->
            {reply, From, Players};
        {call, From, {get_player_nyms, SyncAddresses}} ->
            Nyms =
                lists:map(
                  fun(SyncAddress) ->
                          {value, #player{nym = Nym}} =
                              lists:keysearch(
                                SyncAddress, #player.sync_address, Players),
                          Nym
                  end, SyncAddresses),
            {reply, From, Nyms};
        {call, From, {get_random_player, Nym}} ->
            {reply, From, get_random_player(Nym, Players)};
        {cast, {target_received_message, TargetNym, SourceNym}} ->
            ok = ping(),
            ?daemon_log_tag_fmt(system, "Target received message", []),
            {found, #player{player_serv_pid = TargetPlayerServPid}} =
                get_player(TargetNym, Players),
            player_serv:become_nothing(TargetPlayerServPid),
            {found, #player{player_serv_pid = SourcePlayerServPid}} =
                get_player(SourceNym, Players),
            player_serv:become_nothing(SourcePlayerServPid),
            lists:foreach(
              fun(#player{nym = Nym, player_serv_pid = PlayerServPid})
                    when Nym /= TargetNym andalso Nym /= SourceNym ->
                      player_serv:become_nothing(PlayerServPid);
                 (_) ->
                      ok
              end, Players),
            {noreply, State#state{source = none, target = none}};
        {cast, {received_message, _TargetNym, _SourceNym}} ->
            ok = ping(),
            noreply;
        {cast, analyze} ->
            ok = stats_db:format_analysis(stats_db:analyze(), K),
            noreply;
        %%
        %% Below follows handling of internally generated messages
        %%
        analyze ->
            ok = stats_db:format_analysis(stats_db:analyze(), K),
            noreply;
        simulation_ended ->
            noreply;
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

elect_players(Players, MessageMD5, SourceNym, TargetNym) ->
    ets:insert(player_message, {MessageMD5, true}),
    {found, #player{player_serv_pid = SourcePlayerServPid}} =
        get_player(SourceNym, Players),
    ok = player_serv:become_source(SourcePlayerServPid, TargetNym, MessageMD5),
    {found, #player{player_serv_pid = TargetPlayerServPid}} =
        get_player(TargetNym, Players),
    ok = player_serv:become_target(TargetPlayerServPid, MessageMD5),
    lists:foreach(
      fun(#player{nym = Nym,
                  player_serv_pid = ForwarderPlayerServPid})
            when Nym /= SourceNym andalso Nym /= TargetNym ->
              ok = player_serv:become_forwarder(ForwarderPlayerServPid,
						MessageMD5);
         (#player{nym = _Nym}) ->
              ok
      end, Players),
    ok.

get_random_player(Nym, Players) ->
    N = rand:uniform(length(Players)),
    case lists:nth(N, Players) of
        #player{nym = Nym} ->
            get_random_player(Nym, Players);
        Player ->
            Player
    end.

get_player(_Nym, []) ->
    not_found;
get_player(Nym, [#player{nym = Nym} = Player|_]) ->
    {found, Player};
get_player(Nym, [_|Rest]) ->
    get_player(Nym, Rest).

ping() ->
    spawn(fun() ->
                  Filename =
                      filename:join([code:priv_dir(simulator), "sharp.mp3"]),
                  os:cmd("play " ++ Filename)
          end),
    ok.
