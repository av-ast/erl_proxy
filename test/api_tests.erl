-module(api_tests).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Test get",
      fun test_get/0},
      {"Test delete",
      fun test_delete/0}
    ]
  }.

setup() ->
  {ok, _} = erl_proxy_app:start(),
  erl_proxy_app:config(schedule_pool_interval, 1000),
  erl_proxy_app:config(delay_formula, [{coefficient, 0}, {power, 0}]),
  erl_proxy_app:config(forward_to, "http://localhost/").

teardown(_) ->
  schedule:clear(),
  erl_proxy_app:stop().

test_get() ->
  schedule:clear(),
  ok = meck:new(lhttpc),
  ok = meck:expect(lhttpc, request, lhttpc_request_ags(), {ok, {{500, ""}, [], <<"">>}}),

  request_to_proxy("/", get),
  ?assertEqual(1, schedule:length()),

  {Status, Body} = request_to_proxy("/schedule", get),
  ?assertEqual(200, Status),
  ?assertEqual("{\"length\":1}", Body),

  meck:unload(lhttpc).

test_delete() ->
  schedule:clear(),
  ok = meck:new(lhttpc),
  ok = meck:expect(lhttpc, request, lhttpc_request_ags(), {ok, {{500, ""}, [], <<"">>}}),

  request_to_proxy("/", get),
  ?assertEqual(1, schedule:length()),

  {Status, _Body} = request_to_proxy("/schedule", delete),
  ?assertEqual(204, Status),
  ?assertEqual(0, schedule:length()),

  meck:unload(lhttpc).

request_to_proxy(Path, Method) ->
  Port = erl_proxy_app:config(cowboy_port),
  Url = "http://0.0.0.0:" ++ integer_to_list(Port) ++ Path,
  {ok, {{_, Status, _}, _, Body}} = httpc:request(Method, {Url, []}, [], []),
  {Status, Body}.

 lhttpc_request_ags() ->
  [erl_proxy_app:config(forward_to), '_', '_', '_', '_', '_'].
