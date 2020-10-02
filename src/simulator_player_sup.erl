-module(simulator_player_sup).
-behaviour(supervisor).
-export([start_link/13]). %% Ugh!
-export([init/1]).

%% Exported: start_link

start_link(Name, Password, SyncAddress, SyncPort, TempDir, F,
           GetLocationGenerator, DegreesToMeter, SmtpAddress, SmtpPort,
           SpoolerDir, Pop3Address, Pop3Port) ->
    case supervisor:start_link(
           ?MODULE,
           [Name, Password, SyncAddress, SyncPort, TempDir, F,
            GetLocationGenerator, DegreesToMeter, SmtpAddress, SmtpPort,
            SpoolerDir, Pop3Address, Pop3Port]) of
        {ok, Pid} ->
            Children = supervisor:which_children(Pid),
            ok = player_sup:attach_child(
                   player_serv, [mail_serv, maildrop_serv], Children),
            ok = player_sup:attach_child(
                   player_sync_serv, player_serv, Children),
            ok = player_sup:attach_child(
                   smtp_proxy_serv, player_serv, Children),
            ok = player_sup:attach_child(
                   pop3_proxy_serv, maildrop_serv, Children),
            {ok, Pid};
        Error ->
            Error
    end.

%% Exported: init

init([Name, Password, SyncAddress, SyncPort, TempDir, F, GetLocationGenerator,
      DegreesToMeter, SmtpAddress, SmtpPort, SpoolerDir, Pop3Address,
      Pop3Port]) ->
    PlayerServSpec =
        #{id => player_serv,
          start => {player_serv, start_link,
                    [Name, SyncAddress, SyncPort, TempDir, GetLocationGenerator,
                     DegreesToMeter, true]}},
    PlayerSyncServSpec =
        #{id => player_sync_serv,
          start => {player_sync_serv, start_link,
                    [Name, SyncAddress, SyncPort, F, true]}},
    MailServSpec =
        #{id => mail_serv,
          start => {mail_serv, start_link, [Name, SmtpAddress, SmtpPort]}},
    MaildropServSpec =
        #{id => maildrop_serv,
          start => {maildrop_serv, start_link, [SpoolerDir, true]}},
    SmtpProxyServSpec =
        #{id => smtp_proxy_serv,
          start => {smtp_proxy_serv, start_link,
                    [Name, Password, TempDir, SmtpAddress, SmtpPort]}},
    Pop3ProxyServSpec =
        #{id => pop3_proxy_serv,
          start => {pop3_proxy_serv, start_link,
                    [Name, Password, TempDir, Pop3Address, Pop3Port]}},
    {ok, {#{strategy => one_for_all}, [PlayerServSpec,
                                       PlayerSyncServSpec,
                                       MailServSpec,
                                       MaildropServSpec,
                                       SmtpProxyServSpec,
                                       Pop3ProxyServSpec]}}.
