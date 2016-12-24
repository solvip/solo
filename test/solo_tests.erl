%%%-------------------------------------------------------------------
%%% @author Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2015, Sölvi Páll Ásgeirsson
%%% Created : 17 May 2015 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    solo:start().

stop(_) ->
    solo:stop().

solo_test_() ->
    [ {"call/2,3 executes functions correctly", ?setup(fun call/1) }
    , {"duplicate calls are suppressed", ?setup(fun duplicate_suppression/1) }
    ].

error_handling_test_() ->
    [ {"correctly passes through errors raised by Fun",
       ?setup(fun exception_passthrough/1)}
    , {"kill children that time out",
       ?setup(fun timeouts_are_killed/1)}
    ].


exception_passthrough(_) ->
    [ ?_assertExit(badarg, solo:call(key, fun() -> erlang:exit(badarg) end))
    , ?_assertError(error, solo:call(key, fun() -> erlang:error(error) end))
    , ?_assertThrow(throw, solo:call(key, fun() -> erlang:throw(throw) end))
    ].

call(_) ->
    [ %% call/2 and call/3 behaves as expected
      ?_assertEqual(value, solo:call(key, fun() -> value end))
    , ?_assertEqual(true, solo:call(key, fun() -> true end))
    , ?_assertEqual(<<"xyz">>, solo:call({some, key}, fun() -> <<"xyz">> end, 5000))
    ].

timeouts_are_killed(_) ->
    SendToSelfAndSleep = fun(Pid) ->
				 fun() ->
					 Pid ! self(), 
					 timer:sleep(1000) 
				 end
			 end,
    
    [ %% Assert that solo:call exits with exit:{timeout, _} in the case where
      %% the fun being run exceeds it's timeout
      ?_assertExit({timeout, _}, solo:call(key, SendToSelfAndSleep(self()), 1))

      %% Assert that it gets killed
    , ?_assertNot(is_process_alive(receive_one_message(100)))
    ].

%% Spawn N many child processes, each performing a call with the same key.
%% Ensure that the results for all children are equal.
duplicate_suppression(_) ->
    NumCalls = 1000, %% Number of concurrent, in-flight calls.
    Self = self(),
    lists:foreach(fun(_) -> spawn_expensive_worker(Self, 300) end, lists:seq(1, NumCalls)),
    
    [Ref | Refs] = receive_refs(NumCalls),
    
    [ ?_assertEqual([Ref], lists:usort(Refs)) ].

%% Receive N many refs from the process mailbox.
receive_refs(N) ->
    receive_refs(N, []).

receive_refs(0, Acc) ->
    Acc;
receive_refs(N, Acc) when N > 0 ->
    receive
        Ref when is_reference(Ref) ->
            receive_refs(N-1, [Ref | Acc])
    end.

spawn_expensive_worker(ParentPid, SleepTime) ->
    %% Fun simulates a long-running call.
    Fun = fun() ->
                  Res = solo:call(key, fun() ->
                                               timer:sleep(SleepTime),
                                               erlang:make_ref()
                                       end),
                  ParentPid ! Res
          end,
    spawn_link(Fun).

receive_one_message(TimeoutMs) ->
    receive 
	Msg ->
	    Msg
    after TimeoutMs ->
	    erlang:error(timeout)
    end.
