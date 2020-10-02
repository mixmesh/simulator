-module(stats_db).
-export([new/0]).
-export([message_buffered/1, message_created/3]).
-export([message_duplicate_received/3, message_received/3]).
-export([dump/0, save/0, save/1]).
-export([analyze/0, analyze/1]).

-include_lib("apptools/include/log.hrl").

-define(DISABLED, false).

%% Exported: new

-if(DISABLED).
new() ->
  true.
-else.
new() ->
    ?MODULE ==
        ets:new(?MODULE, [public, named_table, {write_concurrency, true}]).
-endif.

%% Exported: message_buffered

-if(DISABLED).
message_buffered(_Name) ->
    true.
-else.
message_buffered(Name) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_buffered, Timestamp, Name}),
    ets:insert(?MODULE,
               {{message_buffered, Name, erlang:unique_integer()},
                Timestamp}).
-endif.

%% Exported: message_create

-if(DISABLED).
message_created(_MessageId, _SenderName, _Recipient_name) ->
    true.
-else.
message_created(MessageId, SenderName, RecipientName) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_created, Timestamp, SenderName, RecipientName,
              MessageId}),
    ets:insert(?MODULE,
               {{message_created, MessageId}, Timestamp, SenderName,
                RecipientName}).
-endif.

%% Exported: message_duplicate_received

-if(DISABLED).
message_duplicate_received(_MessageId, _SenderName, _RecipientName) ->
    true.
-else.
message_duplicate_received(MessageId, SenderName, RecipientName) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_duplicate_received, Timestamp, RecipientName, SenderName,
              MessageId}),
    ets:insert(?MODULE,
               {{message_duplicate_received, MessageId}, Timestamp, SenderName,
                RecipientName}).
-endif.

%% Exported: message_received

-if(DISABLED).
message_received(_MessageId, _SenderName, _RecipientName) ->
    true.
-else.
message_received(MessageId, SenderName, RecipientName) ->
    Timestamp = os:system_time(millisecond),
    ?dbg_log({message_received, Timestamp, RecipientName, SenderName,
              MessageId}),
    ets:insert(?MODULE,
               {{message_received, MessageId}, Timestamp, SenderName,
                RecipientName}).
-endif.

%% Exported: dump

dump() ->
    ets:tab2list(?MODULE).

%% Exported: save

save() ->
    save("/home/jocke/tmp/dump.dets").

save(Filename) ->
    file:delete(Filename),
    {ok, Name} = dets:open_file(stats, [{file, Filename}]),
    Name = ets:to_dets(?MODULE, Name),
    dets:close(Name).

%% analyze

analyze() ->
    analyze("/home/jocke/tmp/dump.dets").

analyze(Filename) ->
    {ok, Name} = dets:open_file(Filename),
    Tid = ets:new(ram_stats, []),
    Tid = dets:to_ets(Name, Tid),
    perform_analysis(Tid).

perform_analysis(Tid) ->
    io:format("Delivery rate: ~p\n", [delivery_rate(Tid)]),
    ok.

delivery_rate(Tid) ->
    {TotalCreatedMessages, TotalDeliveredMessages, TotalEndToEndDelays} =
        ets:foldl(
          fun(Entry, {CreatedMessages, DeliveredMessages, EndToEndDelays}) ->
                  case Entry of
                      {{message_created, MessageId}, CreateTimestamp,
                       SenderName, RecipientName} ->
                          case ets:match_object(
                                 Tid, {{message_received, MessageId},
                                       '_',
                                       SenderName,
                                       RecipientName}) of
                              [] ->
                                  {CreatedMessages + 1,
                                   DeliveredMessages,
                                   EndToEndDelays};
                              [{_, ReceiveTimestamp, _, _}] ->
                                  {CreatedMessages + 1,
                                   DeliveredMessages + 1,
                                   [ReceiveTimestamp - CreateTimestamp|
                                    EndToEndDelays]}
                          end;
                      _ ->
                          {CreatedMessages, DeliveredMessages, EndToEndDelays}
                  end
          end, {0, 0, []}, ?MODULE),
    if
        TotalDeliveredMessages == 0 ->
            no_messages_delivered;
        true ->
            {TotalDeliveredMessages / TotalCreatedMessages,
             lists:sum(TotalEndToEndDelays) / TotalDeliveredMessages}
    end.
