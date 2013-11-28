-module(storage).
-author("av.astafyev@gmail.com").

-export([push/1, pop/0, queue_length/0]).

-ifdef(TEST).
-define(BACKEND, ets_storage).
-else.
-define(BACKEND, redis_storage).
-endif.

push(Term) ->
  ?BACKEND:push(Term).

pop() ->
  ?BACKEND:pop().

queue_length() ->
  ?BACKEND:queue_length().
