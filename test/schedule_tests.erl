-module(schedule_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

schedule_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
      {"Schedule is empty at the beginning",
      fun test_empty_queue/0},
      {"Pushes new value to schedule",
      fun test_push_value/0},
      {"Pops value from schedule",
      fun test_pop_value/0}
    ]
  }.

setup() ->
  {ok, _} = schedule:start_link([{host, "127.0.0.1"}, {port, 6379}, {namespace, "erl_proxy"}]),
  schedule:clear().

teardown(_) ->
  schedule:clear(),
  schedule:stop().

test_empty_queue() ->
  ?assertEqual(0, schedule:length()).

test_push_value() ->
  ?assertEqual(ok, schedule:add(test_value)),
  ?assertEqual(1, schedule:length()).

test_pop_value() ->
  ?assertEqual(test_value, schedule:retrieve()),
  ?assertEqual(0, schedule:length()),
  ?assertEqual(nothing, schedule:retrieve()).
