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
  application:set_env(lager, error_logger_redirect, false),
  start_web_server(),
  erl_proxy_sup:start_link().

stop(_State) ->
  ok.

config() ->
  {ok, Config} = application:get_env(?MODULE, config),
  Config.

config(Key) ->
  proplists:get_value(Key, config(), undefined).

%% INTERNAL FUNCTIONS

start_deps_applications() ->
  ok = application:start(syntax_tools),
  ok = application:start(compiler),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(crypto),
  ok = application:start(asn1),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = application:start(sasl),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(lhttpc).

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

load_config() ->
  {ok, [Config]} = file:consult(filename:join(["config", "app.config"])),
  application:set_env(?MODULE, config, Config).
