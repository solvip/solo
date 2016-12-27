-module(solo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
    NumWorkers = application:get_env(solo, pool_size, default_num_workers()),
    solo_sup:start_link(NumWorkers).

stop(_State) ->
    ok.

%%===================================================================
%% Internal
%%===================================================================

%% @doc
%% Default to double the number of scheduler threads
%% @end
-spec default_num_workers() -> pos_integer().
default_num_workers() ->
    erlang:system_info(schedulers_online) * 2.
