-module(redis_storage_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

push_pop_test() ->
  redis_storage:start_link(),
  meck:new(redis_storage, [{passthrough, true}]),

  meck:expect(redis_storage, push, fun(_Term) -> ok end),
  meck:expect(redis_storage, pop, fun() -> test_value end),

  redis_storage:push(test_value),
  ?assertEqual(test_value, redis_storage:pop()),
  ?assert(meck:validate(redis_storage)),

  meck:unload(redis_storage),
  redis_storage:stop().
