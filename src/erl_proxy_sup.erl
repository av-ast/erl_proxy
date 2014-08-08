
-module(erl_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_WITH_ARGS(I, Args, Type), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  ScheduleArgs = lists:merge(erl_proxy_app:config(redis), erl_proxy_app:config(schedule)),
  Children = [
              ?CHILD_WITH_ARGS(schedule, ScheduleArgs, worker),
              ?CHILD(request_forwarder, worker)
             ],
  {ok, { {one_for_one, 5, 10}, Children} }.