-module(storage_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

storage_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Storage queue is empty at the beginning",
      fun test_empty_queue/0},
      {"Pushes new value to storage queue",
      fun test_push_value/0},
      {"Pops value from storage queue",
      fun test_pop_value/0}
    ]
  }.

setup() ->
  {ok, _} = ets_storage:start_link().

teardown(_) ->
  ets_storage:stop().

test_empty_queue() ->
  ?assertEqual(0, storage:queue_length()).

test_push_value() ->
  ?assertEqual(ok, storage:push(test_value)),
  ?assertEqual(1, storage:queue_length()).

test_pop_value() ->
  ?assertEqual(test_value, storage:pop()),
  ?assertEqual(0, storage:queue_length()),
  ?assertEqual(empty_queue, storage:pop()).
