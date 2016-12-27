-module(solo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
    DefaultNumWorkers = erlang:system_info(schedulers_online) * 2,
    NumWorkers = application:get_env(solo, pool_size, DefaultNumWorkers),
    solo_sup:start_link(NumWorkers).

stop(_State) ->
    ok.
