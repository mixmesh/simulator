-module(simulator_serv).
-export([start_link/0, stop/0]).
-export([elect_source_and_target/3]).
-export([get_players/0]).
-export([get_player_names/1]).
-export([get_random_player/1]).
-export([meters_to_degrees/1]).
-export([pause_player/0, pause_player/1]).
-export([resume_player/0, resume_player/1]).
-export([stop_generating_mails/0]).
-export([target_received_message/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("player/include/player_serv.hrl").

-define(SIMULATION_TIME, (1000 * 60 * 60 * 10)).

-define(STOP_GENERATING_MAIL_TIME, trunc(?SIMULATION_TIME / 2)).

-define(SYNC_IP_ADDRESS, {127, 0, 0, 1}).
-define(SYNC_PORT, 4000).

-define(SMTP_IP_ADDRESS, {127, 0, 0, 1}).
-define(SMTP_PORT, 16000).

-define(POP3_IP_ADDRESS, {127, 0, 0, 1}).
-define(POP3_PORT, 32000).

-define(PKI_IP_ADDRESS, {127, 0, 0, 1}).
-define(PKI_PORT, 11112).

-define(PICK_RANDOM_SOURCE, false).

-record(state,
        {parent            :: pid(),
         players           :: [#player{}],
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

elect_source_and_target(MessageId, SourceName, TargetName) ->
    serv:cast(?MODULE,
              {elect_source_and_target, MessageId, SourceName, TargetName}).

%% Exported: get_players

get_players() ->
    serv:call(?MODULE, get_players).

%% Exported: get_player_names

get_player_names(SyncAddresses) ->
    serv:call(?MODULE, {get_player_names, SyncAddresses}).

%% Exported: get_random_player

get_random_player(Name) ->
    serv:call(?MODULE, {get_random_player, Name}).

%% Exported: meters_to_degrees

meters_to_degrees(Meters) ->
    serv:call(?MODULE, {meters_to_degrees, Meters}).

%% Exported: pause_player

pause_player() ->
    serv:cast(?MODULE, pause_player).

pause_player(Name) ->
    serv:cast(?MODULE, {pause_player, Name}).

%% Exported: resume_player

resume_player() ->
    serv:cast(?MODULE, resume_player).

resume_player(Name) ->
    serv:cast(?MODULE, {resume_player, Name}).

%% Exported: stop_generating_mails

stop_generating_mails() ->
    serv:cast(?MODULE, stop_generating_mails).

%% Exported: target_received_message

target_received_message(TargetName, SourceName) ->
    serv:cast(?MODULE, {target_received_message, TargetName, SourceName}).

%%
%% Server
%%

init(Parent) ->
    rand:seed(exsss),
    SimulatorModule = config:lookup([simulator, 'data-set']),
    %% Intialize simulator
    %% Area = SimulatorModule:get_area(),
    %% {MinX, MaxX, MinY, MaxY} = Area,
    MetersToDegrees = fun SimulatorModule:meters_to_degrees/1,
    %% NeighbourDistance =
    %% MetersToDegrees(
    %% SimulatorModule:neighbour_distance_in_meters()),
    %% ok = simulator:initialize(MinX, MaxX, MinY, MaxY, NeighbourDistance),
    %% Initialize simulator databases
    true = player_db:new(),
    true = stats_db:new(),
    %% Start simulated players
    LocationIndex = SimulatorModule:get_location_index(),
    {_, _, _, AllPlayers} =
        lists:foldl(
          fun({Name, Opaque}, {SyncPort, SmtpPort, Pop3Port, Players}) ->
                  GetLocationGenerator =
                      fun() ->
                              SimulatorModule:get_location_generator(Opaque)
                      end,
                  DegreesToMeters = fun SimulatorModule:degrees_to_meters/1,
                  PlayerDir =
                      filename:join(["/tmp/obscrete/players", Name, "player"]),
                  TempDir = filename:join([PlayerDir, "temp"]),
                  BufferDir = filename:join([PlayerDir, "buffer"]),
                  Keys = elgamal:generate_key_pair(
                           Name, binary:decode_unsigned(Name)),
                  MaildropSpoolerDir =
                      filename:join([PlayerDir, "maildrop", "spooler"]),
                  LocalPkiServerDataDir =
                      filename:join([PlayerDir, "pki", "data"]),
                  PkiMode = {global, {tcp_only, {?PKI_IP_ADDRESS, ?PKI_PORT}}},
                  %%PkiMode = local,
                  {ok, PlayerSupPid} =
                      supervisor:start_child(
                        simulator_players_sup,
                        [Name, <<"baz">>, {?SYNC_IP_ADDRESS, SyncPort}, TempDir,
                         BufferDir, Keys, ?F, GetLocationGenerator,
                         DegreesToMeters, {?SMTP_IP_ADDRESS, SmtpPort},
                         MaildropSpoolerDir, {?POP3_IP_ADDRESS, Pop3Port},
                         LocalPkiServerDataDir, PkiMode]),
                  {ok, PlayerServPid} =
                      get_child_pid(PlayerSupPid, player_serv),
                  {ok, NodisServPid} =
                      get_child_pid(PlayerSupPid, nodis_serv),
                  %%ok = player_serv:add_dummy_messages(
                  %%       player_serv_pid, rand:uniform(50)),
                  {SyncPort + 1, SmtpPort + 1, Pop3Port + 1,
                   [#player{name = Name,
                            player_serv_pid = PlayerServPid,
                            nodis_serv_pid = NodisServPid,
                            sync_address = {?SYNC_IP_ADDRESS, SyncPort},
                            smtp_address = {?SMTP_IP_ADDRESS, SmtpPort}}|
                    Players]}
          end, {?SYNC_PORT, ?SMTP_PORT, ?POP3_PORT, []}, LocationIndex),
    ok = pick_random_source(AllPlayers),
    %% Order players to start location updating
    lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                          player_serv:start_location_updating(PlayerServPid)
                  end, AllPlayers),
    %% Create timers
    erlang:send_after(?SIMULATION_TIME, self(), simulation_ended),
    erlang:send_after(?STOP_GENERATING_MAIL_TIME, self(), stop_generating_mail),
    ?daemon_tag_log(system, "Master server has been started", []),
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
        {cast, {elect_source_and_target, MessageId, SourceName, TargetName}} ->
            {value, SourcePlayer} =
                lists:keysearch(SourceName, #player.name, Players),
            {value, TargetPlayer} =
                lists:keysearch(TargetName, #player.name, Players),
            ok = elect_source_and_target(
                   Players, MessageId, SourcePlayer, TargetPlayer),
            noreply;
        {call, From, get_players} ->
            {reply, From, Players};
        {call, From, {get_player_names, SyncAddresses}} ->
            Names =
                lists:map(
                  fun(SyncAddress) ->
                          {value, #player{name = Name}} =
                              lists:keysearch(
                                SyncAddress, #player.sync_address, Players),
                          Name
                  end, SyncAddresses),
            {reply, From, Names};
        {call, From, {get_random_player, Name}} ->
            {reply, From, get_random_player(Name, Players)};
        {call, From, {meters_to_degrees, Meters}} ->
            {reply, From, MetersToDegrees(Meters)};
        {cast, pause_player} ->
            lists:foreach(fun(#player{player_serv_pid = PlayerServPid}) ->
                                  player_serv:pause(PlayerServPid)
                          end, Players),
            noreply;
        {cast, {pause_player, Name}} ->
            case get_player(Name, Players) of
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
        {cast, {resume_player, Name}} ->
            case get_player(Name, Players) of
                {found, #player{player_serv_pid = PlayerServPid}} ->
                    player_serv:resume(PlayerServPid),
                    noreply;
                not_found ->
                    noreply
            end;
        {cast, stop_generating_mail} ->
            lists:foreach(
              fun(#player{player_serv_pid = PlayerServPid}) ->
                      player_serv:stop_generating_mail(PlayerServPid)
              end, Players),
            ?daemon_tag_log(system, "No more mails are generated", []),
            noreply;
        {cast, {target_received_message, TargetName, SourceName}} ->
            ok = ping(),
            ?daemon_tag_log(system, "Target received message", []),
            {found, #player{player_serv_pid = TargetPlayerServPid}} =
                get_player(TargetName, Players),
            player_serv:become_nothing(TargetPlayerServPid),
            {found, #player{player_serv_pid = SourcePlayerServPid}} =
                get_player(SourceName, Players),
            player_serv:become_nothing(SourcePlayerServPid),
            lists:foreach(
              fun(#player{name = Name, player_serv_pid = PlayerServPid})
                    when Name /= TargetName andalso Name /= SourceName ->
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
            ?daemon_tag_log(system, "Simulation paused", []),
            ok = stats_db:save(),
            ok = stats_db:analyze(),
            ?daemon_tag_log(system, "Analysis performed", []),
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
  #player{name = SourceName, player_serv_pid = SourcePlayerServPid},
  #player{name = TargetName, player_serv_pid = TargetPlayerServPid}) ->
    ok = player_serv:become_source(SourcePlayerServPid, TargetName, MessageId),
    ?daemon_tag_log(system,
                    "~s has been elected as new source (~w)",
                    [SourceName, MessageId]),
    ok = player_serv:become_target(TargetPlayerServPid, MessageId),
    ?daemon_tag_log(system,
                    "~s has been elected as new target (~w)",
                    [TargetName, MessageId]),
    lists:foreach(
      fun(#player{name = Name,
                  player_serv_pid = ForwarderPlayerServPid})
            when Name /= SourceName andalso Name /= TargetName ->
              ok = player_serv:become_forwarder(
                     ForwarderPlayerServPid, MessageId),
              ?daemon_log(
                 "~s has been elected as new forwarder (~w)",
                 [Name, MessageId]);
         (#player{name = _Name}) ->
              ok
      end, Players),
    ok.

get_random_player(Name, Players) ->
    N = rand:uniform(length(Players)),
    case lists:nth(N, Players) of
        #player{name = Name} ->
            get_random_player(Name, Players);
        Player ->
            Player
    end.

get_player(_Name, []) ->
    not_found;
get_player(Name, [#player{name = Name} = Player|_]) ->
    {found, Player};
get_player(Name, [_|Rest]) ->
    get_player(Name, Rest).

ping() ->
    spawn(fun() ->
                  Filename =
                      filename:join([code:priv_dir(simulator), "sharp.mp3"]),
                  os:cmd("play " ++ Filename)
          end),
    ok.
