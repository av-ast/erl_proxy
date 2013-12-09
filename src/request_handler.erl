-module(request_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Request = prepare_request_for_storage(Req),

  storage:push(Request),

  Req2 = cowboy_req:compact(Req),
  {ok, _} = cowboy_req:reply(erl_proxy_app:config(reply_status), [{<<"connection">>, <<"close">>}], Req2),
  {ok, Req2, State}.

prepare_request_for_storage(Req) ->
  {Method, _} = cowboy_req:method(Req),
  {Url, _}  = cowboy_req:url(Req),
  {Path, _} = cowboy_req:path(Req),
  {QString, _} = cowboy_req:qs(Req),
  {Headers, _} = cowboy_req:headers(Req),
  {ok, Body, _} = cowboy_req:body(Req),

  [
   {method,Method}, {url,Url}, {path, Path}, {qstring, QString}, {headers,Headers}, {body, Body},
   {retry_attempts, erl_proxy_app:config(retry_attempts)}
  ].

terminate(_Reason, _Req, _State) ->
	ok.
