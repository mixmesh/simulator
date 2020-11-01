-module(simulator_serv).
-export([start_link/0, stop/0]).
-export([elect_source_and_target/3]).
-export([get_players/0]).
-export([get_player_nyms/1]).
-export([get_random_player/1]).
-export([meters_to_degrees/1]).
-export([pause_player/0, pause_player/1]).
-export([resume_player/0, resume_player/1]).
-export([target_received_message/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("player/include/player_serv.hrl").

-define(SIMULATION_TIME, (1000 * 60 * 60 * 10)).

-define(SYNC_IP_ADDRESS, {127, 0, 0, 1}).
-define(SYNC_PORT, 4000).

-define(SMTP_IP_ADDRESS, {127, 0, 0, 1}).
-define(SMTP_PORT, 16000).

-define(POP3_IP_ADDRESS, {127, 0, 0, 1}).
-define(POP3_PORT, 32000).

-define(HTTP_IP_ADDRESS, {127, 0, 0, 1}).
-define(HTTP_PORT, 48000).

-define(PKI_IP_ADDRESS, {127, 0, 0, 1}).
-define(PKI_PORT, 11112).

-define(PICK_RANDOM_SOURCE, false).

-record(state,
        {parent :: pid(),
         players :: [#player{}],
         meters_to_degrees :: fun()}).

%% Exported: start_link

start_link() ->
    ?spawn_server_opts(fun init/1,
                       fun message_handler/1,
                       #serv_options{name = ?MODULE}).

%% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%% Exported: elect_source_and_target

elect_source_and_target(MessageId, SourceNym, TargetNym) ->
    serv:cast(?MODULE,
              {elect_source_and_target, MessageId, SourceNym, TargetNym}).

%% Exported: get_players

get_players() ->
    serv:call(?MODULE, get_players).

%% Exported: get_player_nyms

get_player_nyms(SyncAddresses) ->
    serv:call(?MODULE, {get_player_nyms, SyncAddresses}).

%% Exported: get_random_player

get_random_player(Nym) ->
    serv:call(?MODULE, {get_random_player, Nym}).

%% Exported: meters_to_degrees

meters_to_degrees(Meters) ->
    serv:call(?MODULE, {meters_to_degrees, Meters}).

%% Exported: pause_player

pause_player() ->
    serv:cast(?MODULE, pause_player).

pause_player(Nym) ->
    serv:cast(?MODULE, {pause_player, Nym}).

%% Exported: resume_player

resume_player() ->
    serv:cast(?MODULE, resume_player).

resume_player(Nym) ->
    serv:cast(?MODULE, {resume_player, Nym}).

%% Exported: target_received_message

target_received_message(TargetNym, SourceNym) ->
    serv:cast(?MODULE, {target_received_message, TargetNym, SourceNym}).

%%
%% Server
%%

init(Parent) ->
    rand:seed(exsss),
    SimulatorModule = config:lookup([simulator, 'data-set']),
    MetersToDegrees = fun SimulatorModule:meters_to_degrees/1,
    true = player_db:new(),
    true = stats_db:new(),
    %% FIXME: find a place
    %% ets:new(endpoint_reg,[public, named_table, {write_concurrency, true}]),

    %% Start simulated players -
    %% reverse to get player index match port number (and sync order)
    %% LocationIndex = lists:reverse(SimulatorModule:get_location_index()),
    LocationIndex = SimulatorModule:get_location_index(),
    {_, _, _, _, AllPlayers} =
        lists:foldl(
          fun({Nym, Opaque}, {SyncPort, SmtpPort, Pop3Port, HttpPort, Players}) ->
                  ObscreteDir = <<"/tmp/obscrete/players">>,
                  Keys = elgamal:generate_key_pair(
                           Nym, binary:decode_unsigned(Nym)),
                  GetLocationGenerator =
                      fun() ->
                              SimulatorModule:get_location_generator(Opaque)
                      end,
                  DegreesToMeters = fun SimulatorModule:degrees_to_meters/1,
                  %% baz
                  SmtpPasswordDigest =
                      <<237,85,139,97,91,27,175,166,8,177,220,107,101,160,138, 249,172,253,25,226,211,31,248,2,107,122,138,12,220,97, 183,183,182,89,251,10,55,198,134,85,162,164,229,128,66, 117,157,133,43,78,200,39,225,175,154,203,77,253,243,200, 31,87,99,156>>,
                  %% baz
                  Pop3PasswordDigest =
                      <<237,85,139,97,91,27,175,166,8,177,220,107,101,160,138, 249,172,253,25,226,211,31,248,2,107,122,138,12,220,97, 183,183,182,89,251,10,55,198,134,85,162,164,229,128,66, 117,157,133,43,78,200,39,225,175,154,203,77,253,243,200, 31,87,99,156>>,
                  HttpPassword = <<"hello">>,
                  %%PkiMode = {global, <<"baz">>, {tcp_only, {?PKI_IP_ADDRESS, ?PKI_PORT}}},
                  PkiMode = local,
                  {ok, PlayerSupPid} =
                      supervisor:start_child(
                        simulator_players_sup,
                        [#simulated_player_serv_config{
                            obscrete_dir = ObscreteDir,
                            nym = Nym,
                            sync_address = {?SYNC_IP_ADDRESS, SyncPort},
                            keys = Keys,
                            f = ?F,
                            get_location_generator = GetLocationGenerator,
                            degrees_to_meters = DegreesToMeters,
                            smtp_address = {?SMTP_IP_ADDRESS, SmtpPort},
                            smtp_password_digest = SmtpPasswordDigest,
                            pop3_address = {?POP3_IP_ADDRESS, Pop3Port},
                            pop3_password_digest = Pop3PasswordDigest,
                            http_address = {?HTTP_IP_ADDRESS, HttpPort},
                            http_password = HttpPassword,
                            pki_mode = PkiMode}]),
                  {ok, PlayerServPid} =
                      get_child_pid(PlayerSupPid, player_serv),
                  {ok, NodisServPid} =
                      get_child_pid(PlayerSupPid, nodis_serv),
                  {SyncPort + 2, SmtpPort + 1, Pop3Port + 1, HttpPort +1,
                   [#player{nym = Nym,
                            player_serv_pid = PlayerServPid,
                            nodis_serv_pid = NodisServPid,
                            sync_address = {?SYNC_IP_ADDRESS, SyncPort},
                            smtp_address = {?SMTP_IP_ADDRESS, SmtpPort}}|
                    Players]}
          end, {?SYNC_PORT, ?SMTP_PORT, ?POP3_PORT, ?HTTP_PORT, []}, LocationIndex),
    ok = pick_random_source(AllPlayers),
    %% Order players to start location updating
    lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                          player_serv:start_location_updating(PlayerServPid)
                  end, AllPlayers),
    %% Create timers
    erlang:send_after(?SIMULATION_TIME, self(), simulation_ended),
    ?daemon_log_tag_fmt(system, "Master server has been started", []),
    {ok, #state{parent = Parent,
                players = AllPlayers,
                meters_to_degrees = MetersToDegrees}}.

get_child_pid(SupervisorPid, ChildId) ->
    {value, {_Id, ChildPid, _Type, _Modules}} =
        lists:keysearch(ChildId, 1, supervisor:which_children(SupervisorPid)),
    {ok, ChildPid}.

-if(PICK_RANDOM_SOURCE).
pick_random_source(Players) ->
    #player{player_serv_pid = PlayerServPid} =
        lists:nth(rand:uniform(length(Players)), Players),
    player_serv:pick_as_source(PlayerServPid).
-else.
pick_random_source(_Players) ->
    ok.
-endif.

message_handler(#state{parent = Parent,
                       players = Players,
                       meters_to_degrees = MetersToDegrees}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {cast, {elect_source_and_target, MessageId, SourceNym, TargetNym}} ->
            {value, SourcePlayer} =
                lists:keysearch(SourceNym, #player.nym, Players),
            {value, TargetPlayer} =
                lists:keysearch(TargetNym, #player.nym, Players),
            ok = elect_source_and_target(
                   Players, MessageId, SourcePlayer, TargetPlayer),
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
        {call, From, {meters_to_degrees, Meters}} ->
            {reply, From, MetersToDegrees(Meters)};
        {cast, pause_player} ->
            lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                                  player_serv:pause(PlayerServPid)
                          end, Players),
            noreply;
        {cast, {pause_player, Nym}} ->
            case get_player(Nym, Players) of
                {found, #player{player_serv_pid = PlayerServPid}} ->
                    player_serv:pause(PlayerServPid),
                    noreply;
                not_found ->
                    noreply
            end;
        {cast, resume_player} ->
            lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                                  player_serv:resume(PlayerServPid)
                          end, Players),
            noreply;
        {cast, {resume_player, Nym}} ->
            case get_player(Nym, Players) of
                {found, #player{player_serv_pid = PlayerServPid}} ->
                    player_serv:resume(PlayerServPid),
                    noreply;
                not_found ->
                    noreply
            end;
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
            ok = pick_random_source(Players),
            noreply;
        %%
        %% Below follows handling of internally generated messages
        %%
        simulation_ended ->
            lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                                  player_serv:pause(PlayerServPid)
                          end, Players),
            ?daemon_log_tag_fmt(system, "Simulation paused", []),
            ok = stats_db:save(),
            ok = stats_db:analyze(),
            ?daemon_log_tag_fmt(system, "Analysis performed", []),
            noreply;
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

elect_source_and_target(
  Players, MessageId,
  #player{nym = SourceNym, player_serv_pid = SourcePlayerServPid},
  #player{nym = TargetNym, player_serv_pid = TargetPlayerServPid}) ->
    ok = player_serv:become_source(SourcePlayerServPid, TargetNym, MessageId),
    ?daemon_log_tag_fmt(system,
                        "~s has been elected as new source (~w)",
                        [SourceNym, MessageId]),
    ok = player_serv:become_target(TargetPlayerServPid, MessageId),
    ?daemon_log_tag_fmt(system,
                        "~s has been elected as new target (~w)",
                        [TargetNym, MessageId]),
    lists:foreach(
      fun(#player{nym = Nym,
                  player_serv_pid = ForwarderPlayerServPid})
            when Nym /= SourceNym andalso Nym /= TargetNym ->
              ok = player_serv:become_forwarder(
                     ForwarderPlayerServPid, MessageId),
              ?daemon_log_fmt(
                 "~s has been elected as new forwarder (~w)",
                 [Nym, MessageId]);
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
