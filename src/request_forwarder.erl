-module(request_forwarder).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { tref }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, TRef} = timer:send_interval(erl_proxy_app:config(delay_between_requests), process_request),
  {ok, #state{tref = TRef}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(process_request, State) ->
  case storage:pop() of
    empty_queue ->
      ok;
    Request ->
      case forward_request(Request) of
        {ok, Status} ->
          lager:debug("[Request was forwarded successfully]: ~p", [Status]),
          ok;
        {bad_response, Response} ->
          % TODO: need to parse Response for status and more information
          lager:debug("[Bad response]: ~p", [Response]),
          retry_request(Request);
        {request_failed, Reason} ->
          lager:debug("[Request failed]: ~p", [Reason]),
          retry_request(Request) % TODO: do we really need to retry request later?
      end
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, #state{tref = TRef} = _State) ->
  timer:cancel(TRef),
  ok.

%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

forward_request(Request) ->
  Request2 = utils:deep_binary_to_list(Request),
  ForwardURI = erl_proxy_app:config(forward_to),

  Response = http_request(ForwardURI, Request2),
  case Response of
    {ok, {{_HttpVersion, Status, _StatusMessage}, _Headers, _Body}} when Status div 100 =:= 2 ->
      {ok, Status};
    {error, Reason} ->
      {request_failed, Reason};
    _ ->
      {bad_response, Response}
  end.

http_request(ForwardURI, Request) ->
  Method = http_verb_to_atom(proplists:get_value(method, Request, "GET")),
  Headers = proplists:get_value(headers, Request, []),
  RequestURI = proplists:get_value(url, Request, ""),
  ContentType = proplists:get_value(content_type, Request, "text/plain"),
  Body = proplists:get_value(body, Request, ""),
  HTTPOptions = [
                 {timeout, erl_proxy_app:config(request_timeout)},
                 {connect_timeout, erl_proxy_app:config(connection_timeout)}
                ],

  {ok, {FSchema, FUserInfo, FHost, FPort, _FPath, _FQueryString}} = uri:parse(ForwardURI),
  {ok, {_RSchema, _RUserInfo, _RHost, _RPort, RPath, RQueryString}} = uri:parse(RequestURI),

  URI = uri:to_string({FSchema, FUserInfo, FHost, FPort, RPath, RQueryString}),

  FullHost = lists:flatten(uri:full_host_iolist({http, [], FHost, FPort, [], ""}, [])),

  Headers2 = lists:keyreplace("host", 1, Headers, {"host", FullHost}),
  Headers3 = lists:keyreplace("user-agent", 1, Headers2, {"user-agent", erl_proxy_app:config(user_agent)}),

  case lists:member(Method, [get, head]) of
    true ->
      lager:debug("[REQUEST]: ~p", [[Method, URI, Headers3]]),
      httpc:request(Method, {URI, Headers3}, HTTPOptions, []);
    _ ->
      lager:debug("[REQUEST]: ~p", [[Method, URI, Headers3, Body]]),
      httpc:request(Method, {URI, Headers3, ContentType, Body}, HTTPOptions, [])
  end.

%%
%% @spec http_verb_to_atom(Verb) -> atom() | bad_match
http_verb_to_atom(Verb) ->
  case Verb of
    "GET"   -> get;
    "POST"  -> post;
    "PUT"   -> put;
    "PATCH" -> put;
    "HEAD"  -> head;
    _       -> bad_match
  end.

retry_request(Request) ->
  case erl_proxy_app:config(retry_requests) of
    true -> storage:push(Request);
    _ -> ok
  end.
