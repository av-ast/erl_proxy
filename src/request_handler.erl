-module(request_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([request_handler/2]).

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

  storage:push(Request),

  Req2 = cowboy_req:compact(Req),
  {ok, _} = cowboy_req:reply(erl_proxy_app:config(reply_status), [{<<"connection">>, <<"close">>}], Req2),
  {halt, Req2, State}.

prepare_request_for_storage(Req) ->
  {Method, _} = cowboy_req:method(Req),
  {Url, _}  = cowboy_req:url(Req),
  {Path, _} = cowboy_req:path(Req),
  {QString, _} = cowboy_req:qs(Req),
  {ContentType, _} = cowboy_req:header(<<"content-type">>, Req, <<"text/plain">>),
  {Headers, _} = cowboy_req:headers(Req),
  {ok, Body, _} = cowboy_req:body(Req),

  [{method,Method}, {url,Url}, {path, Path}, {qstring, QString}, {headers,Headers}, {body, Body},{content_type, ContentType}].
