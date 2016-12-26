%%%-------------------------------------------------------------------
%%% @author Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2016, Sölvi Páll Ásgeirsson
%%% @doc
%%% Benchmarking suite for solo
%%% @end
%%% Created : 26 Dec 2016 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo_bench).

%% API
-export([ short/3
	]).

%% Private API
-export([exec_call/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Perform many short(as fast as it goes) calls via solo, returning the time taken
%% in seconds.
%% We'll spawn NumConcurrent processes, each doing NumCalls over NumKeys different keys.
%% The total number of calls will be NumConcurrent * NumCalls.
%% @end
-spec short(NumKeys, NumProcs, CallsPerProc) -> {TimeTakenSec, CallsPerSecond} when
      NumKeys :: pos_integer(),
      NumProcs :: pos_integer(),
      CallsPerProc :: pos_integer(),
      TimeTakenSec :: float(),
      CallsPerSecond :: float().
short(NumKeys, NumProcs, CallsPerProc) ->
    {MicroSec, ok} = 
	timer:tc(fun() ->
			 spawn_link_many(NumProcs, ?MODULE, exec_call, [self(), CallsPerProc, NumKeys]),
			 
			 %% We should receive NumConcurrent many messages.  Wait for it.
			 wait_for_done_messages(NumProcs)
		 end),
    TimeTakenSec = MicroSec / 1000 / 1000,
    CallsPerSecond = (CallsPerProc * NumProcs) / TimeTakenSec,
    
    {TimeTakenSec, CallsPerSecond}.
    
%% @doc
%% Execute N many solo:call/3s.  When done, send a message to ParentPid.
%% @end
-spec exec_call(ParentPid, N, NumKeys) -> ok when
      ParentPid :: pid(),
      N :: pos_integer(),
      NumKeys :: pos_integer().
exec_call(ParentPid, 0, _NumKeys) ->
    ParentPid ! {self(), done},
    ok;
exec_call(ParentPid, N, NumKeys) ->
    _ = solo:call({some, key, rand:uniform(NumKeys)}, fun make_ref/0),
    exec_call(ParentPid, N-1, NumKeys).
    

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
