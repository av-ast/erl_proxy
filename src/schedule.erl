-module(schedule).
-behavior([gen_server]).

-author("av.astafyev@gmail.com").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([clear/0, add/1, add/2, retrieve/0, retrieve/1, length/0]).

-record(state, {
          redis_client :: pid() | undefined
       }).

%% public API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, RedisClient} = eredis:start_link(),
  {ok, #state{redis_client = RedisClient}}.

stop() ->
  gen_server:cast(?MODULE, stop).

%% public client API

%% @spec clear() -> ok
clear() ->
  gen_server:call(?MODULE, clear).

%% @spec add(Term) -> ok
%%
add(Term) ->
  PerformAt = utils:ts(),
  add(Term, PerformAt).

%% @spec add(Term, PerformAt) -> ok
%%
add(Term, PerformAt) when is_binary(Term) ->
  gen_server:cast(?MODULE, {add, Term, PerformAt});
add(Term, PerformAt) ->
  add(term_to_binary(Term), PerformAt).

%% @spec retrieve() -> term() | nothing
%%
retrieve() ->
  Timestamp = utils:ts(),
  retrieve(Timestamp).

%% @spec retrieve(Timestamp) -> term() | nothing
%%
retrieve(Timestamp) ->
  gen_server:call(?MODULE, {retrieve, Timestamp}).

%% @spec length() -> integer()
%%
length() ->
  gen_server:call(?MODULE, length).

%% gen_server callbacks

handle_call(clear, _From, #state{redis_client = RedisClient} = State) ->
  {ok, _} = eredis:q(RedisClient, ["DEL", sorted_set_key_name()]),
  {ok, _} = eredis:q(RedisClient, ["DEL", hash_key_name()]),
  {reply, ok, State};

handle_call({retrieve, Timestamp}, _From, #state{redis_client = RedisClient} = State) ->
  Result = case eredis:q(RedisClient, ["ZRANGEBYSCORE", sorted_set_key_name(), "-inf", Timestamp, "LIMIT", 0, 1]) of
    {ok, []} -> nothing;
    {ok, [TermId]} ->
      {ok, <<"1">>} = eredis:q(RedisClient, ["ZREM", sorted_set_key_name(), TermId]),
      {ok, Res} = eredis:q(RedisClient, ["HGET", hash_key_name(), TermId]),
      {ok, <<"1">>} = eredis:q(RedisClient, ["HDEL", hash_key_name(), TermId]),
      binary_to_term(Res)
  end,

  {reply, Result, State};
handle_call(length, _From, #state{redis_client = RedisClient} = State) ->
  {ok, Length} = eredis:q(RedisClient, ["ZCOUNT", sorted_set_key_name(), "-inf", "+inf"]),
  {reply, binary_to_integer(Length), State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({add, Term, PerformAt}, #state{redis_client = RedisClient} = State) ->
  TermId = utils:ts(),
  {ok, <<"1">>} = eredis:q(RedisClient, ["HSET", hash_key_name(), TermId, Term]),
  {ok, <<"1">>} = eredis:q(RedisClient, ["ZADD", sorted_set_key_name(), PerformAt, TermId]),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

hash_key_name() ->
  "erl_proxy" ++ ":" ++ "hash".

sorted_set_key_name() ->
  "erl_proxy" ++ ":" ++ "sorted_set".
