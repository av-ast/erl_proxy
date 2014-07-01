-module(erl_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0, config/1, config/2]).

start() ->
  {ok, _} = application:ensure_all_started(erl_proxy).

stop() ->
  Apps = [erl_proxy, uri, inets, eredis, cowboy, lhttpc, ssl, ranch, lager, goldrush],
  [application:stop(App) || App <- Apps].

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  application:set_env(lager, error_logger_redirect, false),
  start_web_server(),
  erl_proxy_sup:start_link().

stop(_State) ->
  ok.

config(Key) ->
  {ok, Value} = application:get_env(erl_proxy, Key),
  Value.

config(Key, Value) ->
  application:set_env(erl_proxy, Key, Value).

%% INTERNAL FUNCTIONS

start_web_server() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/schedule", schedule_handler, []},
      {'_', request_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, erl_proxy_app:config(cowboy_acceptors_num),
                              [{port, config(cowboy_port)}],
                              [
                                {env, [{dispatch, Dispatch}]}
                              ]).
