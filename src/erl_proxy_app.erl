-module(erl_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

-define(HTTP_PORT, 8888).

start() ->
    application:start(erl_proxy).
  
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  start_deps_applications(),
  start_web_server(),
  erl_proxy_sup:start_link().

stop(_State) ->
  ok.

%% INTERNAL FUNCTIONS

start_deps_applications() ->
  ok = lager:start(),
  ok = inets:start(),
  ok = application:start(crypto),
  ok = application:start(sasl),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy).

start_web_server() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {'_', request_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, 100, [{port, ?HTTP_PORT}], [
    {env, [{dispatch, Dispatch}]}
  ]).
