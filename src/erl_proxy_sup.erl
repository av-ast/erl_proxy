
-module(erl_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-ifdef(TEST).
-define(STORAGE_BACKEND, ets_storage).
-else.
-define(STORAGE_BACKEND, redis_storage).
-endif.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Children = [
              ?CHILD(?STORAGE_BACKEND, worker),
              ?CHILD(request_forwarder, worker)
             ],
  {ok, { {one_for_one, 5, 10}, Children} }.
