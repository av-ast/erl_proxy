-module(utils).

-export([ts/0, ts_str/0]).

ts() ->
  {Megasecs, Secs, Microsecs} = now(),
  Microsecs + 1000000 * (Secs + 1000000 * Megasecs).

ts_str() ->
  integer_to_list(ts()).