-module(simulator).
-export([initialize/5]).
-export([add_players/1, update_players/1]).
-export([nplayer/1, scale_factor/0, send_messages/4]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("player/include/player_serv.hrl").

-define(IS_NOTHING, 0).
-define(IS_FORWARDER, 1).
-define(IS_SOURCE, 2).
-define(IS_TARGET, 3).

%% Exported: initialize

initialize(MinLongitude, MaxLongitude,
           MinLatitude, MaxLatitude,
           NeighbourDistance) ->
    simulator_nif:initialize(MinLongitude, MaxLongitude,
                             MinLatitude, MaxLatitude,
                             NeighbourDistance).

%% Exported: add_players

add_players([]) ->
    ok;
add_players(NewPlayers) ->
    ConvertedNewPlayers =
        lists:map(fun({Nym, X, Y}) ->
                          {?b2l(Nym), X, Y}
                  end, NewPlayers),
    simulator_nif:add_players(ConvertedNewPlayers).

%% Exported: update_players

update_players([]) ->
    ok;
update_players(UpdatedPlayers) ->
    ConvertedUpdatedPlayers =
        lists:map(fun({Nym, UpdatedValues}) ->
                          {?b2l(Nym), convert_updated_values(UpdatedValues)}
                  end, UpdatedPlayers),
    simulator_nif:update_players(ConvertedUpdatedPlayers).

convert_updated_values([]) ->
    [];
convert_updated_values([{neighbours, Neighbours}|Rest]) ->
    UpdatedNeighbours = lists:map(fun(Nym) -> ?b2l(Nym) end, Neighbours),
    [{neighbours, UpdatedNeighbours}|convert_updated_values(Rest)];
convert_updated_values([{is_zombie, true}|Rest]) ->
    [{is_zombie, 1}|convert_updated_values(Rest)];
convert_updated_values([{is_zombie, false}|Rest]) ->
    [{is_zombie, 0}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, is_nothing}|Rest]) ->
    [{pick_mode, ?IS_NOTHING}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode,
                         {is_forwarder, {message_not_in_buffer, _}}}|Rest]) ->
    [{pick_mode, ?IS_NOTHING}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode,
                         {is_forwarder, {message_in_buffer, _}}}|Rest]) ->
    [{pick_mode, ?IS_FORWARDER}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, {is_source, {_, _}}}|Rest]) ->
    [{pick_mode, ?IS_SOURCE}|convert_updated_values(Rest)];
convert_updated_values([{pick_mode, {is_target, _}}|Rest]) ->
    [{pick_mode, ?IS_TARGET}|convert_updated_values(Rest)];
convert_updated_values([UpdatedValue|Rest]) ->
    [UpdatedValue|convert_updated_values(Rest)].

%% Exported: nplayer

nplayer(DefaultValue) ->
    case os:getenv("NPLAYER") of
        false ->
            DefaultValue;
        ToString ->
            ?l2i(ToString)
    end.

%% Exported: scale_factor

scale_factor() ->
    case os:getenv("SCALEFACTOR") of
        false ->
            1;
        ScaleFactorString ->
            ?l2i(ScaleFactorString)
    end.

%% Exported: send_messages

send_messages(Players, ScaleFactor, TargetNym, ResendTime) ->
    lists:foreach(
      fun(#player{nym = Nym}) when TargetNym == Nym ->
              ok;
         (#player{player_serv_pid = PlayerServPid}) ->
              Payload = ?i2b(erlang:unique_integer([positive])),
              spawn(
                fun() ->
                        io:format(">"),
                        player_serv:send_message(
                          PlayerServPid, TargetNym, Payload),
                        io:format("<")
                end)
      end, Players),
    timer:apply_after(trunc(ResendTime / ScaleFactor), ?MODULE,
                      send_messages,
                      [Players, ScaleFactor, TargetNym, ResendTime]).
