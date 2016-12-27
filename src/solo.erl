%%%-------------------------------------------------------------------
%%% @author Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2015, Sölvi Páll Ásgeirsson
%%% Created : 14 May 2015 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         stop/0,
         call/2,
         call/3
        ]).

%% Private API
-export([runner/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start() ->
    application:ensure_all_started(solo).

stop() ->
    application:stop(solo).

%% @doc 
%% Equivalent to call(Key, Fun, 5000).
%% @end
-spec call(Key::term(), Fun::fun()) -> term() | none().
call(Key, Fun) ->
    call(Key, Fun, 5000).        

%% @doc 
%% Call Fun with duplicate call suppression.
%% Only a single instance of Fun can be in flight at any time for Key.
%% If subsequent callers call call with the same Key, they receive a copy of Fun's return value.
%%
%% If Fun doesn't return within Timeout milliseconds, it will be killed and 
%% call/3 exits with reason {timeout, _}.
%%
%% If Fun throws an exception, it will get reraised to all clients.
%% @end
-spec call(Key::term(), Fun::fun(), Timeout::non_neg_integer()) -> term() | none().
call(Key, Fun, Timeout) when is_function(Fun), Timeout > 0 ->
    case solo_reg:get_member(Key) of
        {ok, Pid} ->
            case gen_server:call(Pid, {call, Key, Fun, Timeout}, Timeout) of
                {local, Value} ->
                    Value;
                {exception, Class, Reason, Stacktrace} ->
                    erlang:raise(Class, Reason, Stacktrace)
            end;
	{error, noproc} ->
            erlang:exit({noproc, {solo, call, [Key, Fun, Timeout]}})
    end.

%%%===================================================================
%%% Private API
%%%===================================================================

%% @doc
%% runner/3 is responsible for running a user supplied function for Key.
%% It catches all exceptions and sends a message to Parent with the return value of Fun.
%% In the case where Fun returns normally, the message sent is {local, Value}.
%% If Fun throws an exception, the message sent is {exception, Class, Reason, Stacktrace}.
%% @end
-spec runner(Key::term(), Fun::function(), Parent::pid()) -> ok.
runner(Key, Fun, Parent) ->
    Res = try Fun() of
              Value -> {local, Value}
          catch Class:Reason ->
                  {exception, Class, Reason, erlang:get_stacktrace()}
          end,
    Parent ! {return, self(), Key, Res},
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Construct a client key.
-define(client_key(Key), {solo_client, Key}).
-define(runner_key(Key), {solo_runner, Key}).

init([]) ->
    solo_reg:register(),
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

%% If there is no duplicate of function keyed by Key running, spawn one and register
%% the calling process as a client waiting for the result.
%% If there is a duplication function running, simply register the calling process as waiting.
handle_call({call, Key, Fun, Timeout}, Client, State) ->
    case erlang:get(?client_key(Key)) of
        undefined -> 
            RunnerPid = spawn_link(?MODULE, runner, [Key, Fun, self()]),
            TimerRef = erlang:send_after(Timeout, self(), {timeout, RunnerPid, Key}),
            erlang:put(?runner_key(RunnerPid), {Key, TimerRef}),
            erlang:put(?client_key(Key), [Client]);
        Clients ->
            erlang:put(?client_key(Key), [Client | Clients])
    end,
    
    {noreply, State}.

handle_cast(_Msg, State) ->
    {stop, no_cast_clause, State}.

%% Send return values to clients waiting for them.
handle_info({return, RunnerPid, Key, Result}, State) ->
    %% Send the response to waiting clients.
    Clients = erlang:erase(?client_key(Key)),
    multireply(Clients, Result),

    %% Cancel the timeout timer, remove the runner as a running process.
    {Key, TimerRef} = erlang:erase(?runner_key(RunnerPid)),
    erlang:cancel_timer(TimerRef),
    
    {noreply, State};

%% Handle cleanup of timed out runner funs.
handle_info({timeout, RunnerPid, Key}, State) ->
    case erlang:erase(?runner_key(RunnerPid)) of
        undefined ->
            %% Process already exited.
            ok;
        {Key, _TimerRef} ->
            %% Send kill, as running fun could otherwise trap exits.
            exit(RunnerPid, kill),
            erlang:erase(?client_key(Key))
    end,
    {noreply, State};

%% Handle exits of linked processes.
handle_info({'EXIT', _RunnerPid, _Reason}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% No need to kill runners here, as the link will cascade to them.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Like gen_server:reply/2, but for replying to a list of clients.
%% @end
-spec multireply(Clients, Reply) -> ok when
      Clients :: [{pid(), term()}],
      Reply :: term().
multireply([], _Reply) ->
    ok;
multireply([Client | Rest], Reply) ->
    gen_server:reply(Client, Reply),
    multireply(Rest, Reply).
