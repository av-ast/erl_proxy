-module(too_many_requests_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

erl_proxy_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Test",
      fun test/0}
    ]
  }.

setup() ->
  {ok, _} = erl_proxy_app:start(),
  erl_proxy_app:config(schedule_pool_interval, 100),
  erl_proxy_app:config(delay_formula, [{coefficient, 0}, {power, 0}]),
  erl_proxy_app:config(forward_to, "http://localhost/"),
  erl_proxy_app:config(max_rpm_per_host, 1).
  % SEE THIS LINE                       ^^^

teardown(_) ->
  schedule:clear(),
  statistics:clear(),
  erl_proxy_app:stop().

test() ->
  ok = meck:new(lhttpc),
  ok = meck:expect(lhttpc, request, lhttpc_request_ags(), {ok, {{200, ""}, [], <<"">>}}),

  Status1 = ?MODULE:request_to_proxy(),
  ?assertEqual(200, Status1),

  Status2 = ?MODULE:request_to_proxy(),
  ?assertEqual(429, Status2),

  RetryCount = 1,
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
