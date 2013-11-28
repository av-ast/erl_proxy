-module(ets_storage).
-behavior(gen_server).

-author("av.astafyev@gmail.com").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/0]).
-export([push/1, pop/0, queue_length/0]).

-record(state, {
          table :: atom() | integer() | undefined
       }).

%% public API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{table = ets:new(ets_storage_table, [duplicate_bag, private])}}.

stop() ->
  gen_server:cast(?MODULE, stop).

%% public client API

push(Term) when is_binary(Term) ->
  gen_server:cast(?MODULE, {push, Term});
push(Term) ->
  push(term_to_binary(Term)).

pop() ->
  gen_server:call(?MODULE, pop).

queue_length() ->
  gen_server:call(?MODULE, queue_length).

%% gen_server callbacks

handle_call(pop, _From, #state{table = Tab} = State) ->
  Result = case ets:first(Tab) of
             '$end_of_table' -> empty_queue;
             Key -> case ets:lookup(Tab, Key) of
                      [Object] ->
                        ets:delete_object(Tab, Object),
                        binary_to_term(erlang:element(2, Object));
                      _ -> object_not_found
                    end
           end,
  {reply, Result, State};
handle_call(queue_length, _From, #state{table = Tab} = State) ->
  {reply, ets:info(Tab, size), State};
handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({push, Term}, #state{table = Tab} = State) ->
  ets:insert(Tab, {utils:ts(), Term}),
  {noreply, State};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
