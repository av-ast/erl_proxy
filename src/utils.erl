-module(utils).

-export([ts/0, unix_ts/0, ts_str/0, ip_string_from_cowboy_req/1, current_minute_number/0, redis_withscores_to_tuple_list/1, deep_binary_to_list/1]).

ts() ->
  {Megasecs, Secs, Microsecs} = now(),
  Microsecs + 1000000 * (Secs + 1000000 * Megasecs).

unix_ts() ->
  {Megasecs, Secs, _} = now(),
  Megasecs * 1000000 + Secs.

ts_str() ->
  integer_to_list(ts()).

current_minute_number() ->
  unix_ts() div 60.

ip_string_from_cowboy_req(Req) ->
  {{IpAddress, _},_} = cowboy_req:peer(Req),
  inet:ntoa(IpAddress).

% return reversed list
redis_withscores_to_tuple_list([Member, Score | T]) ->
  ScoreInt = list_to_integer(binary_to_list(Score)),
  [{Member, ScoreInt} | redis_withscores_to_tuple_list(T)];
redis_withscores_to_tuple_list(_) ->
  [].

%%
%% @spec deep_binary_to_list(List) -> list()
deep_binary_to_list([]) -> [];
deep_binary_to_list([{Key, [{V1, V2}|_] = Pairs}|Tail]) when is_atom(Key), is_binary(V1), is_binary(V2) ->
  NewValue = [{binary_to_list(Val1), binary_to_list(Val2)} || {Val1, Val2} <- Pairs],
  [{Key, NewValue} | deep_binary_to_list(Tail)];
deep_binary_to_list([{Key, Value}|Tail]) when is_atom(Key), is_binary(Value) ->
  [{Key, binary_to_list(Value)} | deep_binary_to_list(Tail)];
deep_binary_to_list([{Key, Value}|Tail]) when is_atom(Key) ->
  [{Key, Value} | deep_binary_to_list(Tail)].
