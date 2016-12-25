%%%-------------------------------------------------------------------
%%% @author Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2016, Sölvi Páll Ásgeirsson
%%% Created : 25 Dec 2016 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo_reg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    {ok, Pid} = solo_reg:start_link(),
    Pid.

stop(Pid) ->
    gen_server:stop(Pid).

get_member_exits_if_server_is_stopped_test() ->
    ?assertExit({noproc, {solo_reg, get_member, [whatever]}},
		solo_reg:get_member(whatever)).

solo_reg_test_() ->
    [ {"error is returned if nothing is registered", ?setup(fun nothing_is_registered/1) }
    , {"register and get member do their thing", ?setup(fun register_and_get_member/1) }
    ].

nothing_is_registered(_) ->
    ?_assertEqual({error, noproc}, solo_reg:get_member(whatever)).

register_and_get_member(_) ->
    [ ?_assertEqual(ok, solo_reg:register())
    , ?_assertEqual({ok, self()}, solo_reg:get_member(whatever))
    ].
