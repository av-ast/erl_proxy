-module(redis_storage).
-behavior(gen_server).

-author("av.astafyev@gmail.com").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([push/1, pop/0, queue_length/0]).

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

%% @spec push(Term) -> ok
%%
push(Term) when is_binary(Term) ->
  gen_server:cast(?MODULE, {push, Term});
push(Term) ->
  push(term_to_binary(Term)).

%% @spec pop() -> term() | empty_queue
%%
pop() ->
  gen_server:call(?MODULE, pop).

%% @spec queue_length() -> integer()
%%
queue_length() ->
  gen_server:call(?MODULE, queue_length).

%% gen_server callbacks

handle_call(pop, _From, #state{redis_client = RedisClient} = State) ->
  Result = case eredis:q(RedisClient, ["LPOP", queue]) of
             {ok, undefined} -> empty_queue;
             {ok, Res} -> binary_to_term(Res)
           end,
  {reply, Result, State};
handle_call(queue_length, _From, #state{redis_client = RedisClient} = State) ->
  {ok, Length} = eredis:q(RedisClient, ["LLEN", queue]),
  {reply, binary_to_integer(Length), State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({push, Term}, #state{redis_client = RedisClient} = State) ->
  {ok, _} = eredis:q(RedisClient, ["RPUSH", queue, Term]),
  {noreply, State};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
