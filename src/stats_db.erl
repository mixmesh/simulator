-module(stats_db).
-export([new/0]).
-export([message_buffered/1, message_created/3, message_duplicate_received/3,
         message_received/3, messages_relayed/1]).
-export([dump/0, save/0, save/1]).
-export([analyze/0, analyze/1, format_analysis/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("stats_db.hrl").

%% Exported: new

new() ->
    ?MODULE = ets:new(?MODULE, [public, named_table,
                                {write_concurrency, true}]),
    Timestamp = os:system_time(millisecond),
    true = ets:insert(?MODULE, {messages_relayed, 0}),
    ets:insert(?MODULE, {start_time, Timestamp}).

%% Exported: message_buffered

message_buffered(Nym) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_buffered, Timestamp, Nym}),
    ets:insert(?MODULE,
               {{message_buffered, Nym, erlang:unique_integer()},
                Timestamp}).

%% Exported: message_create

message_created(MessageMD5, SenderNym, RecipientNym) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_created, Timestamp, SenderNym, RecipientNym,
              MessageMD5}),
    ets:insert(?MODULE,
               {{message_created, MessageMD5}, Timestamp, SenderNym,
                RecipientNym}).

%% Exported: message_duplicate_received

message_duplicate_received(MessageMD5, SenderNym, RecipientNym) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_duplicate_received, Timestamp, RecipientNym, SenderNym,
              MessageMD5}),
    ets:insert(?MODULE,
               {{message_duplicate_received, MessageMD5}, Timestamp, SenderNym,
                RecipientNym}).

%% Exported: message_received

message_received(MessageMD5, SenderNym, RecipientNym) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_received, Timestamp, RecipientNym, SenderNym,
              MessageMD5}),
    ets:insert(?MODULE,
               {{message_received, MessageMD5}, Timestamp, SenderNym,
                RecipientNym}).

%% Exported: messages_relayed

messages_relayed(N) ->
    [{_, M}] = ets:lookup(?MODULE, messages_relayed),
    ets:insert(?MODULE, {messages_relayed, N + M}).

%% Exported: dump

dump() ->
    ets:tab2list(?MODULE).

%% Exported: save

save() ->
    save("/home/jocke/tmp/dump.dets").

save(Filename) ->
    file:delete(Filename),
    {ok, Nym} = dets:open_file(stats, [{file, Filename}]),
    Nym = ets:to_dets(?MODULE, Nym),
    dets:close(Nym).

%% analyze

analyze(Filename) ->
    {ok, Nym} = dets:open_file(Filename),
    Tid = ets:new(ram_stats, []),
    Tid = dets:to_ets(Nym, Tid),
    perform_analysis(Tid).

analyze() ->
    perform_analysis(?MODULE).

perform_analysis(Tid) ->
    [{_, StartTime}] = ets:lookup(?MODULE, start_time),
    CurrentTime = os:system_time(millisecond),
    {CreatedMessages, DeliveredMessages, DeliveryDelays} =
        ets:foldl(
          fun(Entry, {CreatedMessages, DeliveredMessages, DeliveryDelays}) ->
                  case Entry of
                      {{message_created, MessageMD5}, CreateTimestamp,
                       SenderNym, RecipientNym} ->
                          case ets:match_object(
                                 Tid, {{message_received, MessageMD5},
                                       '_',
                                       SenderNym,
                                       RecipientNym}) of
                              [] ->
                                  {CreatedMessages + 1, DeliveredMessages,
                                   DeliveryDelays};
                              [{_, ReceiveTimestamp, _, _}] ->
                                  {CreatedMessages + 1, DeliveredMessages + 1,
                                   [ReceiveTimestamp - CreateTimestamp|
                                    DeliveryDelays]}
                          end;
                      _ ->
                          {CreatedMessages, DeliveredMessages, DeliveryDelays}
                  end
          end, {0, 0, []}, ?MODULE),
    [{_, MessagesRelayed}] = ets:lookup(?MODULE, messages_relayed),
    #stats_db_analysis{
       start_time = StartTime,
       current_time = CurrentTime,
       created_messages = CreatedMessages,
       delivered_messages = DeliveredMessages,
       delivery_delays = DeliveryDelays,
       relayed_messages = MessagesRelayed}.

%% format_analysis

format_analysis(#stats_db_analysis{
                   start_time = StartTime,
                   current_time = CurrentTime,
                   created_messages = CreatedMessages,
                   delivered_messages = DeliveredMessages,
                   delivery_delays = DeliveryDelays,
                   relayed_messages = RelayedMessages}) ->
    ScaleFactor =
        case os:getenv("SCALEFACTOR") of
            false ->
                1;
            ScaleFactorString ->
                ?l2i(ScaleFactorString)
        end,
    SimulatorRunTimeInMinutes =  (CurrentTime - StartTime) / 1000 / 60,
    io:format("\nSimulator run time: ~s minutes\n",
              [number_to_string(SimulatorRunTimeInMinutes)]),
    ScaledSimulatorRunTimeInMinutes = SimulatorRunTimeInMinutes * ScaleFactor,
    io:format("Scaled simulator run time: ~s minutes\n",
              [number_to_string(ScaledSimulatorRunTimeInMinutes)]),
    case CreatedMessages of
        0 ->
            ok;
        _ ->
            io:format("Created messages: ~w\n", [CreatedMessages]),
            io:format("Delivered messages: ~w\n", [DeliveredMessages]),
            io:format("Relayed messages: ~w\n", [RelayedMessages]),
            io:format("Delivery rate: ~s\n",
                      [number_to_string(
                         DeliveredMessages / CreatedMessages)])
    end,
    case DeliveredMessages of
        0 ->
            ok;
        _ ->
            io:format("Average delivery delay: ~s minutes\n",
                      [number_to_string(
                         lists:sum(DeliveryDelays) * ScaleFactor /
                             DeliveredMessages / 1000 / 60)]),
            SortedDeliveryDelays = lists:sort(DeliveryDelays),
            MeanDeliveryDelay =
                lists:nth(round(length(SortedDeliveryDelays) / 2),
                          SortedDeliveryDelays),
            io:format("Mean delivery delay: ~s minutes\n",
                      [number_to_string(
                         MeanDeliveryDelay * ScaleFactor / 1000 / 60)])
    end,
    ok.

number_to_string(Integer) when is_integer(Integer) ->
    ?i2l(Integer);
number_to_string(Float) when is_float(Float) ->
    float_to_list(Float, [{decimals, 4}, compact]).
