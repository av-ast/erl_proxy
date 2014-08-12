-module(statistics).
-behavior(gen_server).

-author("m.kuzmin@darkleaf.ru").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([add_request_from_host/1, requests_from_host/1, requests_from_all_hosts/0, clear/0]).

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
  {ok, RedisClient} = hierdis:connect(Host, Port),
  {ok, #state{redis_client = RedisClient, redis_namespace = Namespace}}.

stop() ->
  gen_server:cast(?MODULE, stop).

%% public client API

%% @spec add_request_from_host(Host) -> ok
add_request_from_host(Host) ->
  gen_server:cast(?MODULE, {add_request_from_host, Host}).

%% @spec requests_from_host(Host) -> integer()
requests_from_host(Host) ->
  gen_server:call(?MODULE, {requests_from_host, Host}).

%% @spec requestsFromAllHosts() -> [{Host, Numbers_of_requests} | T]
requests_from_all_hosts() ->
  gen_server:call(?MODULE, requests_from_all_hosts).

%% @spec clear() -> ok
clear() ->
  gen_server:call(?MODULE, clear).

%% gen_server callbacks

handle_call({requests_from_host, Host}, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, RequestsCountBinary} = hierdis:command(RedisClient, ["ZSCORE", sorted_set_key_name(Namespace), Host]),
  RequestsCount = list_to_integer(binary_to_list(RequestsCountBinary)),
  {reply, RequestsCount, State};
handle_call(requests_from_all_hosts, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, Result} = hierdis:command(RedisClient, ["ZREVRANGEBYSCORE", sorted_set_key_name(Namespace), "+inf", "-inf", "WITHSCORES"]),
  {reply, utils:redis_withscores_to_tuple_list(Result), State};
handle_call(clear, _From, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  {ok, Keys} = hierdis:command(RedisClient, ["KEYS", sorted_set_key_name_mask(Namespace)]),
  {ok, _} = hierdis:command(RedisClient, ["DEL", Keys]),
  {reply, ok, State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({add_request_from_host, Host}, #state{redis_client = RedisClient, redis_namespace = Namespace} = State) ->
  Key = sorted_set_key_name(Namespace),
  TimeStamp = integer_to_list(sorted_set_expire_at()),
  {ok, _} = hierdis:command(RedisClient, ["ZINCRBY", Key, "1", Host]),
  {ok, _} = hierdis:command(RedisClient, ["EXPIREAT", Key, TimeStamp]),
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

sorted_set_key_name_mask(Namespace) ->
  Namespace ++ ":statistics_sorted_set:*".

sorted_set_key_name(Namespace) ->
  Namespace ++ ":statistics_sorted_set:" ++ integer_to_list(utils:current_minute_number()).

sorted_set_expire_at() ->
  Ttl = 2 * 60,
  utils:current_minute_number() * 60 + Ttl.
