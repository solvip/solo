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

simple_call_test_() ->
    {"simple calls are correctly handled",
     ?setup(fun() ->
                    ?assertEqual(value, solo:call(key, fun() -> value end)),
                    ?assertNotEqual(false, solo:call(key, fun() -> true end))
            end)}.

duplicate_call_test_() ->
    {"duplicate calls are suppressed",
     ?setup(fun duplicate_suppression/0)}.

error_handling_test_() ->
    {"solo correctly passes through errors raised by Fun",
     ?setup(fun exception_passthrough/0)}.

exception_passthrough() ->
    ?assertExit(badarg, solo:call(key, fun() -> erlang:exit(badarg) end)),
    ?assertError(error, solo:call(key, fun() -> erlang:error(error) end)),
    ?assertThrow(throw, solo:call(key, fun() -> erlang:throw(throw) end)).

%% Spawn N many child processes, each performing a call with the same key.
%% Ensure that the results for all children are equal.
duplicate_suppression() ->
    NumCalls = 1000, %% Number of concurrent, in-flight calls.
    Self = self(),
    lists:foreach(fun(_) -> spawn_expensive_worker(Self) end, lists:seq(1, NumCalls)),
    
    [Ref | Refs] = receive_refs(NumCalls),
    
    lists:foldl(fun(R, R1) ->
                        ?assertEqual(R, R1),
                        Ref
                end, Ref, Refs).

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

spawn_expensive_worker(ParentPid) ->
    %% Fun simulates a long-running call.
    Fun = fun() ->
                  Res = solo:call(key, fun() ->
                                               timer:sleep(1000),
                                               erlang:make_ref()
                                       end),
                  ParentPid ! Res
          end,
    spawn_link(Fun).
