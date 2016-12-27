%%%-------------------------------------------------------------------
%%% @author Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2016, Sölvi Páll Ásgeirsson
%%% @doc
%%% Benchmarking suite for solo.
%%% @end
%%% Created : 26 Dec 2016 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo_bench).

%% API
-export([ format/1
	, main/1
	, profile_run_and_format/4
	, run/4
	, run_and_format/4
	]).

%% Private API
-export([exec_call/4]).

-record(results, { time_taken :: float()
		 , total_calls :: pos_integer()
		 , calls_per_second :: float()
		 }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Main function, so that we can escriptize and run this easily.
%% The benchmark parameters are chosen as to take roughly 3-5 seconds on my MacBook.
%% @end
main([]) ->
    {ok,_} = application:ensure_all_started(solo),

    %% Test the case where we have many short running calls against 1, 10, 100, 100 keys,
    %% and with very high concurrency.
    run_and_format(1, 1000, 1000, 0),
    run_and_format(10, 1000, 1000, 0),
    run_and_format(100, 1000, 1000, 0),
    run_and_format(1000, 1000, 1000, 0),
    run_and_format(1000, 10000, 100, 0),
    
    %% Test the case where we have many medium-long running calls against 1, 10, 100, 1000 keys,
    %% and with very high concurrency.
    run_and_format(1, 2500, 400, 10),
    run_and_format(10, 2500, 400, 10),
    run_and_format(100, 2500, 400, 10),
    run_and_format(1000, 2500, 400, 10),
    run_and_format(1000, 25000, 40, 10),
    
    %% Test the case where we have many long running calls against 1, 10, 100, 1000 keys,
    %% and with very high concurrency
    run_and_format(1, 5000, 80, 100),
    run_and_format(10, 5000, 80, 100),
    run_and_format(100, 5000, 80, 100),
    run_and_format(1000, 5000, 80, 100),
    run_and_format(1000, 50000, 20, 100);
main(["profile"]) ->
    {ok, _} = application:ensure_all_started(solo),
    
    profile_run_and_format(100, 1000, 1000, 0).


%% @doc
%% Perform a run, but with profiling of solo.
%% @end
-spec profile_run_and_format(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) -> ok when
      NumKeys :: pos_integer(),
      NumProcs :: pos_integer(),
      CallsPerProc :: pos_integer(),
      MaxDelayMs :: non_neg_integer().
profile_run_and_format(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) ->
    SoloProcs = lists:map(fun({_, Pid, _, _}) -> Pid end,
			  supervisor:which_children(solo_sup)),

    profiling = eprof:start_profiling(SoloProcs),
    run_and_format(NumKeys, NumProcs, CallsPerProc, MaxDelayMs),
    eprof:stop_profiling(),
    eprof:analyze(total).

%% @doc
%% Same as run; but pretty prints the input parameters and the results
%% @end
-spec run_and_format(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) -> ok when
      NumKeys :: pos_integer(),
      NumProcs :: pos_integer(),
      CallsPerProc :: pos_integer(),
      MaxDelayMs :: pos_integer().
run_and_format(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) ->
    io:format("~p procs, ~p calls each, ~p keys, up to ~p ms latency per call:~n",
	      [NumProcs, CallsPerProc, NumKeys, MaxDelayMs]),
    format(run(NumKeys, NumProcs, CallsPerProc, MaxDelayMs)),
    io:format("~n").

%% @doc
%% Run a benchmark against solo, returning a results record.
%% We'll spawn NumConcurrent processes, each doing NumCalls over NumKeys different keys.
%% The total number of calls will be NumConcurrent * NumCalls.
%% If MaxDelayMs is greater than zero, the spawned processes will sleep for a random
%% time between 0 - MaxDelayMs; intended to simulate long-running calls.
%% @end
-spec run(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) -> #results{} when
      NumKeys :: pos_integer(),
      NumProcs :: pos_integer(),
      CallsPerProc :: pos_integer(),
      MaxDelayMs :: pos_integer().
run(NumKeys, NumProcs, CallsPerProc, MaxDelayMs) ->
    {MicroSec, ok} = 
	timer:tc(fun() ->
			 Args = [self(), CallsPerProc, NumKeys, MaxDelayMs],
			 spawn_link_many(NumProcs, ?MODULE, exec_call, Args),
			 
			 %% We should receive NumConcurrent many messages.  Wait for it.
			 wait_for_done_messages(NumProcs)
		 end),

    TimeTakenSec = MicroSec / 1000 / 1000,
    TotalCalls = CallsPerProc * NumProcs,
    CallsPerSecond = trunc(TotalCalls / TimeTakenSec),
    
    #results{ time_taken = TimeTakenSec
	    , total_calls = TotalCalls
	    , calls_per_second = CallsPerSecond
	    }.

%% @doc
%% Execute N many solo:call/3s.  When done, send a message to ParentPid.
%% @end
-spec exec_call(ParentPid, N, NumKeys, MaxDelayMs) -> ok when
      ParentPid :: pid(),
      N :: pos_integer(),
      NumKeys :: pos_integer(),
      MaxDelayMs :: pos_integer().
exec_call(ParentPid, N, NumKeys, MaxDelayMs) ->
    Fun = fun() ->
		  timer:sleep(rand:uniform(MaxDelayMs + 1) - 1),
		  make_ref()
	  end,
    exec_call1(ParentPid, N, NumKeys, Fun).
    
exec_call1(ParentPid, 0, _NumKeys, _Fun) ->
    ParentPid ! {self(), done},
    ok;
exec_call1(ParentPid, N, NumKeys, Fun) ->
    _ = solo:call({some, key, rand:uniform(NumKeys)}, Fun),
    exec_call1(ParentPid, N-1, NumKeys, Fun).

%% @doc
%% Format results
%% @end
-spec format(#results{}) -> ok.
format(#results{time_taken = TimeTakenSec, total_calls = TotalCalls, calls_per_second = CPS}) ->
    io:format("#results{  time_taken = ~.3f, total_calls = ~p, calls_per_second = ~p }~n",
	      [TimeTakenSec, TotalCalls, CPS]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_link_many(0, _Mod, _Fun, _Args) ->
    ok;
spawn_link_many(N, Mod, Fun, Args) ->
    _ = spawn_link(Mod, Fun, Args),
    spawn_link_many(N-1, Mod, Fun, Args).


wait_for_done_messages(0) ->
    ok;
wait_for_done_messages(N) ->
    receive
	{_Pid, done} ->
	    wait_for_done_messages(N-1)
    end.
