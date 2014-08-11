-module(request_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  CanProcess = can_process(Req),
  {ok, ResultReq} = if
    CanProcess -> usual_request(Req);
    true -> too_many_requests(Req)
  end,

  CompactedRequest = cowboy_req:compact(ResultReq),
  {ok, CompactedRequest, State}.

usual_request(Req) ->
  RequestForStore = prepare_request_for_storage(Req),
  schedule:add(RequestForStore),
  cowboy_req:reply(erl_proxy_app:config(reply_status), Req).

too_many_requests(Req) ->
  cowboy_req:reply(429, Req).

prepare_request_for_storage(Req) ->
  {Method, _} = cowboy_req:method(Req),
  {Url, _}  = cowboy_req:url(Req),
  {Path, _} = cowboy_req:path(Req),
  {QString, _} = cowboy_req:qs(Req),
  {Headers, _} = cowboy_req:headers(Req),
  {ok, Body, _} = cowboy_req:body(Req),

  NewReq = [
   {method,Method}, {url,Url}, {path, Path}, {qstring, QString}, {headers,Headers}, {body, Body},
   {retry_count, 0}
  ],

  utils:deep_binary_to_list(NewReq).

can_process(Req) ->
  {{IpAddress, _},_} = cowboy_req:peer(Req),
  IpString = inet:ntoa(IpAddress),
  statistics:add_request_from_host(IpString),
  statistics:requests_from_host(IpString) =< erl_proxy_app:config(max_rpm_per_host).

terminate(_Reason, _Req, _State) ->
	ok.
