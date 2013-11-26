-module(redis_gateway).
-behavior(gen_server).

-author("av.astafyev@gmail.com").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([get/1, set/2]).

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

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
  gen_server:cast(?MODULE, {set, Key, Value}).

%% gen_server callbacks

handle_call({get, Key}, _From, #state{redis_client = RedisClient} = State) ->
  Response = eredis:q(RedisClient, ["GET", Key]),
  {reply, Response, State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({set, Key, Value}, #state{redis_client = RedisClient} = State) ->
  {ok, _} = eredis:q(RedisClient, ["SET", Key, Value]),
  {noreply, State};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%% helper methods
