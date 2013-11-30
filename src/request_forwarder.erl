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
        {ok, _Status} ->
          lager:debug("[Request was forwarded successfully]: ~p", [Request]),
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
  % TODO: replace headers[host] with config[forward_to]

  Response = httpc:request(
   http_verb_to_atom(proplists:get_value(method, Request2, "GET")),
   {
     erl_proxy_app:config(forward_to),
     proplists:get_value(headers, Request2, []),
     proplists:get_value(content_type, Request2, "text/plain"),
     proplists:get_value(body, Request2, "")
   },
   [
    {timeout, erl_proxy_app:config(request_timeout)},
    {connect_timeout, erl_proxy_app:config(connection_timeout)}
   ],
   []
  ),

  case Response of
    {ok, {{_HttpVersion, Status, _StatusMessage}, _Headers, _Body}} when Status div 100 =:= 2 ->
      {ok, Status};
    {error, Reason} ->
      {request_failed, Reason};
    _ ->
      {bad_response, Response}
  end.

%%
%% @spec http_verb_to_atom(Verb) -> atom() | bad_verb
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
