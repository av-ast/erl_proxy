-module(schedule).
-behavior(gen_server).

-author("av.astafyev@gmail.com").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([clear/0, add/1, add/2, retrieve/0, retrieve/1, length/0]).

-record(state, {
          redis_client :: pid() | undefined,
          redis_namespace :: list(),
          compression_level :: integer()
       }).

%% public API

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
  Namespace = proplists:get_value(namespace, Args),
  CompressionLevel = proplists:get_value(compression_level, Args, 0),
  {ok, RedisClient} = hierdis:connect(Host, Port),
  {ok, #state{redis_client = RedisClient, redis_namespace = Namespace, compression_level = CompressionLevel}}.

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
add(Term, PerformAt) ->
  gen_server:cast(?MODULE, {add, Term, PerformAt}).

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
  Commands = [["DEL", sorted_set_key_name(Namespace)],
              ["DEL", hash_key_name(Namespace)]],
  {ok, _} = hierdis:transaction(RedisClient, Commands),
  {reply, ok, State};

handle_call({retrieve, Timestamp}, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  TimestampStr = integer_to_list(Timestamp),
  ResultForTermId = hierdis:command(RedisClient, ["ZRANGEBYSCORE", sorted_set_key_name(Namespace), "-inf", TimestampStr, "LIMIT", "0", "1"]),
  Result = case ResultForTermId of
    {ok, []} -> nothing;
    {ok, [TermIdStr]} ->
      Commands = [["HGET", hash_key_name(Namespace), TermIdStr],
                  ["HDEL", hash_key_name(Namespace), TermIdStr],
                  ["ZREM", sorted_set_key_name(Namespace), TermIdStr]],
      {ok, [BinaryData | _OtherResults]} = hierdis:transaction(RedisClient, Commands),
      binary_to_term(BinaryData)
  end,
  {reply, Result, State};
handle_call(length, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, Length} = hierdis:command(RedisClient, ["ZCARD", sorted_set_key_name(Namespace)]),
  {reply, Length, State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({add, Term, PerformAt}, #state{redis_client = RedisClient, redis_namespace = Namespace, compression_level = CompressionLevel} = State) ->
  PerformAtStr = integer_to_list(PerformAt),
  BinaryData = term_to_binary(Term, [{compressed, CompressionLevel}]),
  TermIdStr = utils:ts_str(),
  Commands = [["HSET", hash_key_name(Namespace), TermIdStr, BinaryData],
              ["ZADD", sorted_set_key_name(Namespace), PerformAtStr, TermIdStr]],
  {ok, _} = hierdis:transaction(RedisClient, Commands),
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
