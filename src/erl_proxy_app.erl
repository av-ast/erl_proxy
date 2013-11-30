-module(erl_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, config/0, config/1]).

start() ->
  application:start(erl_proxy).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  load_config(),
  start_deps_applications(),
  start_web_server(),
  {ok, _} = erl_proxy_sup:start_link().

stop(_State) ->
  ok.

config() ->
  {ok, Config} = application:get_env(?MODULE, config),
  Config.

config(Key) ->
  proplists:get_value(Key, config(), undefined).

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

  {ok, _} = cowboy:start_http(http, 100, [{port, config(cowboy_port)}], [
    {env, [{dispatch, Dispatch}]}
  ]).

load_config() ->
  {ok, [Config]} = file:consult(filename:join(["config", "app.config"])),
  application:set_env(?MODULE, config, Config).
