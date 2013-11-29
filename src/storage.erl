-module(storage).
-author("av.astafyev@gmail.com").

-export([push/1, pop/0, queue_length/0]).

-ifdef(TEST).
-define(BACKEND, ets_storage).
-else.
-define(BACKEND, redis_storage).
-endif.

%% @spec push(Term) -> ok
%%
push(Term) ->
  ?BACKEND:push(Term).

%% @spec pop() -> term() | empty_queue
%%
pop() ->
  ?BACKEND:pop().

%% @spec queue_length() -> integer()
%%
queue_length() ->
  ?BACKEND:queue_length().
