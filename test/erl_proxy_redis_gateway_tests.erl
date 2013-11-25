-module(erl_proxy_redis_gateway_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

get_set_value_test() ->
  redis_gateway:start_link(),
  meck:new(redis_gateway, [{passthrough, true}]),

  meck:expect(redis_gateway, set, fun(_Key, _Value) -> ok end),
  meck:expect(redis_gateway, get, fun(_Key) -> {ok, <<"value">>} end),

  redis_gateway:set(key1, <<"value">>),
  ?assertEqual({ok, <<"value">>}, redis_gateway:get(key1)),
  ?assert(meck:validate(redis_gateway)),

  meck:unload(redis_gateway),
  redis_gateway:stop().
