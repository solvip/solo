-module(solo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 1000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NumWorkers = application:get_env(solo, pool_size, default_num_workers()),
    WorkerSpec = lists:map(fun worker_spec/1, lists:seq(1, NumWorkers)),
    
    {ok, { {one_for_all, 3, 60}, [?CHILD(solo_reg, worker)] ++ WorkerSpec 
         } }.

worker_spec(N) ->
    {N, {solo, start_link, []}, permanent, 1000, worker, [solo]}.

%% @doc
%% Default to double the number of scheduler threads
%% @end
-spec default_num_workers() -> pos_integer().
default_num_workers() ->
    erlang:system_info(schedulers_online) * 2.
