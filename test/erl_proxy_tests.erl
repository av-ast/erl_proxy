-module(erl_proxy_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

erl_proxy_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Test standart flow",
      fun test_standart_flow/0},
      {"Test retry",
      fun test_retry/0}
    ]
  }.

setup() ->
  {ok, _} = erl_proxy_app:start(),
  erl_proxy_app:config(schedule_pool_interval, 500),
  erl_proxy_app:config(delay_formula, [{coefficient, 1}, {power, 0}]),
  erl_proxy_app:config(forward_to, "http://localhost/").

teardown(_) ->
  schedule:clear(),
  erl_proxy_app:stop().

test_standart_flow() ->
  schedule:clear(),
  ok = meck:new(lhttpc),
  ok = meck:expect(lhttpc, request, lhttpc_request_ags(), {ok, {{200, ""}, [], <<"">>}}),

  Status = ?MODULE:request_to_proxy(),
  ?assertEqual(200, Status),

  RetryCount = 1,
  Timeout = calc_timeout(RetryCount),
  ok = meck:wait(RetryCount, lhttpc, request, 6, Timeout),
  meck:unload(lhttpc).

test_retry() ->
  ok = meck:new(lhttpc),
  ResponseSeq = meck:seq([{ok, {{500, ""}, [], <<"">>}},
                          {ok, {{200, ""}, [], <<"">>}}]),
  ok = meck:expect(lhttpc, request, lhttpc_request_ags(), ResponseSeq),

  Status = ?MODULE:request_to_proxy(),
  ?assertEqual(200, Status),

  RetryCount = 2,
  Timeout = calc_timeout(RetryCount),
  ok = meck:wait(RetryCount, lhttpc, request, 6, Timeout),
  meck:unload(lhttpc).

request_to_proxy() ->
  Port = erl_proxy_app:config(cowboy_port),
  Url = "http://0.0.0.0:" ++ integer_to_list(Port),
  {ok, {{_, Status, _}, _, _}} = httpc:request(Url),
  Status.

 lhttpc_request_ags() ->
  [erl_proxy_app:config(forward_to), '_', '_', '_', '_', '_'].

calc_timeout(RetryCount) ->
  (RetryCount + 1) * erl_proxy_app:config(schedule_pool_interval).
