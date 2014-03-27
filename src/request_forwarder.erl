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

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Timer = erlang:send_after(1, self(), process_request),
  {ok, Timer}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(process_request, OldTimer) ->
  erlang:cancel_timer(OldTimer),

  StoredRequest = schedule:retrieve(),

  Timer = case StoredRequest of
    nothing ->
      erlang:send_after(erl_proxy_app:config(schedule_pool_interval), self(), process_request);
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
      end,
      erlang:send_after(1, self(), process_request)
  end,
  {noreply, Timer};

handle_info(_Info, State) ->
  {noreply, State}.

%%
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, Timer) ->
  erlang:cancel_timer(Timer),
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
  ForwardURI = erl_proxy_app:config(forward_to),
  Response = http_request(ForwardURI, Request),

  case Response of
    {ok, {{StatusCode, _ReasonPhrase}, _Hdrs, _ResponseBody}} when StatusCode div 100 =/= 5 ->
      {ok, StatusCode};
    {error, Reason} ->
      {request_failed, Reason};
    _ ->
      {bad_response, Response}
  end.

http_request(ForwardURI, Request) ->
  Method = proplists:get_value(method, Request, "GET"),
  Headers = proplists:get_value(headers, Request, []),
  RequestURI = proplists:get_value(url, Request, ""),
  Body = proplists:get_value(body, Request, ""),
  Timeout = erl_proxy_app:config(request_timeout),
  Options = [
             {connect_timeout, erl_proxy_app:config(connection_timeout)}
            ],

  {ok, {FSchema, FUserInfo, FHost, FPort, _FPath, _FQueryString}} = uri:parse(ForwardURI),
  {ok, {_RSchema, _RUserInfo, _RHost, _RPort, RPath, RQueryString}} = uri:parse(RequestURI),

  URI = uri:to_string({FSchema, FUserInfo, FHost, FPort, RPath, RQueryString}),
  FullHost = lists:flatten(uri:full_host_iolist({http, [], FHost, FPort, [], ""}, [])),
  Headers2 = lists:keyreplace(<<"host">>, 1, Headers, {<<"host">>, FullHost}),
  Headers3 = lists:keyreplace(<<"user-agent">>, 1, Headers2, {<<"user-agent">>, erl_proxy_app:config(user_agent)}),

  lager:debug("[REQUEST]: ~p", [[Method, URI, Headers3, Body]]),
  lhttpc:request(URI, Method, Headers3, Body, Timeout, Options).

retry_request(Request) ->
  RetryCount = proplists:get_value(retry_count, Request, 0),
  RetryAttempts = erl_proxy_app:config(retry_attempts),

  case RetryCount of
    N when N < RetryAttempts ->
      NewRequest = lists:keyreplace(retry_count, 1, Request, {retry_count, N + 1}),
      PerformAt = utils:ts() + calc_delay(N),
      schedule:add(NewRequest, PerformAt);
    _ -> ok
  end.

calc_delay(RetryCount) ->
  Formula = erl_proxy_app:config(delay_formula),
  Coefficient = proplists:get_value(coefficient, Formula),
  Power = proplists:get_value(power, Formula),

  DelayInSec = round(Coefficient * math:pow(RetryCount + 1, Power)),
  DelayInSec * 1000000.
