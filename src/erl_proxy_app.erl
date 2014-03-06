-module(erl_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, start_with_deps/0, config/1]).

start_with_deps() ->
  ok = lager:start(),
  Apps = [crypto, asn1, public_key, ssl, lhttpc, ranch, cowlib, cowboy,
         eredis, inets, uri, sync, erl_proxy],
  [ok = application:start(App) || App <- Apps].

start() ->
  application:start(erl_proxy).

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

%% INTERNAL FUNCTIONS

start_web_server() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {'_', request_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, erl_proxy_app:config(cowboy_acceptors_num),
                              [{port, config(cowboy_port)}],
                              [
                                {env, [{dispatch, Dispatch}]}
                              ]).
