-module(storage_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

push_pop_test() ->
  erl_proxy_sup:start_link(),

  ?assertEqual(0, storage:queue_length()),

  ?assertEqual(ok, storage:push(test_value)),
  ?assertEqual(1, storage:queue_length()),

  ?assertEqual(test_value, storage:pop()),
  ?assertEqual(0, storage:queue_length()),

  ?assertEqual(empty_queue, storage:pop()).
