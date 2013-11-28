-module(client_request_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([request_handler/2]).

-define(REPLY_STATUS, 200).

init({tcp,http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"HEAD">>], Req, State}.

content_types_provided(Req, State) ->
	{[{<<"application/json">>, request_handler}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, request_handler}], Req, State}.

request_handler(Req, State) ->
  Request = prepare_request_for_storage(Req),
  lager:debug("Request: ~p", [Request]),

  redis_storage:push(Request),

  Req2 = cowboy_req:compact(Req),
  {ok, _} = cowboy_req:reply(?REPLY_STATUS, [{<<"connection">>, <<"close">>}], Req2),
  {halt, Req2, State}.

prepare_request_for_storage(Req) ->
  {Method, _} = cowboy_req:method(Req),
  {Host, _} = cowboy_req:host(Req),
  {Url, _}  = cowboy_req:url(Req),
  {Path, _} = cowboy_req:path(Req),
  {QString, _} = cowboy_req:qs(Req),
  {Headers, _} = cowboy_req:headers(Req),
  {ok, Body, _} = cowboy_req:body(Req),

  [{method, Method}, {host, Host}, {url, Url}, {path, Path}, {qstring, QString}, {headers, Headers}, {body, Body}].
