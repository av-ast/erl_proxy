-module(schedule).
-behavior([gen_server]).

-author("av.astafyev@gmail.com").

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([clear/0, add/1, add/2, retrieve/0, retrieve/1, length/0]).

-record(state, {
          redis_client :: pid() | undefined,
          redis_namespace = "erl_proxy"
       }).

%% public API

start_link() ->
  start_link([]).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
  Host = proplists:get_value(host, Args, "127.0.0.1"),
  Port = proplists:get_value(port, Args, 6379),
  Namespace = proplists:get_value(namespace, Args, "erl_proxy"),
  {ok, RedisClient} = hierdis:connect(Host, Port),
  {ok, #state{redis_client = RedisClient, redis_namespace = Namespace}}.

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

handle_call(clear, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, _} = hierdis:command(RedisClient, ["DEL", sorted_set_key_name(Namespace)]),
  {ok, _} = hierdis:command(RedisClient, ["DEL", hash_key_name(Namespace)]),
  {reply, ok, State};

handle_call({retrieve, Timestamp}, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  TimestampStr = integer_to_list(Timestamp),
  Result = case hierdis:command(RedisClient, ["ZRANGEBYSCORE", sorted_set_key_name(Namespace), "-inf", TimestampStr, "LIMIT", "0", "1"]) of
    {ok, []} -> nothing;
    {ok, [TermIdStr]} ->
      {ok, 1} = hierdis:command(RedisClient, ["ZREM", sorted_set_key_name(Namespace), TermIdStr]),
      {ok, Res} = hierdis:command(RedisClient, ["HGET", hash_key_name(Namespace), TermIdStr]),
      {ok, 1} = hierdis:command(RedisClient, ["HDEL", hash_key_name(Namespace), TermIdStr]),
      binary_to_term(Res)
  end,

  {reply, Result, State};
handle_call(length, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, Length} = hierdis:command(RedisClient, ["ZCOUNT", sorted_set_key_name(Namespace), "-inf", "+inf"]),
  {reply, Length, State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({add, Term, PerformAt}, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  TermIdStr = integer_to_list(utils:ts()),
  PerformAtStr = integer_to_list(PerformAt),
  {ok, 1} = hierdis:command(RedisClient, ["HSET", hash_key_name(Namespace), TermIdStr, Term]),
  {ok, 1} = hierdis:command(RedisClient, ["ZADD", sorted_set_key_name(Namespace), PerformAtStr, TermIdStr]),
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

hash_key_name(Namespace) ->
  Namespace ++ ":" ++ "hash".

sorted_set_key_name(Namespace) ->
  Namespace ++ ":" ++ "sorted_set".
