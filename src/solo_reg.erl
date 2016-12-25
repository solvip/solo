%%%-------------------------------------------------------------------
%%% @author Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%% @copyright (C) 2015, Sölvi Páll Ásgeirsson
%%% Created : 17 May 2015 by Sölvi Páll Ásgeirsson <solvip@gmail.com>
%%%-------------------------------------------------------------------
-module(solo_reg).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_member/1,
         register/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, solo_reg_ets).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Find the pid responsible for Key or {error, noproc} if no process has
%% been registered.
%% @end
-spec get_member(Key::term()) -> {ok, pid()} | {error, noproc}.
get_member(Key) ->
    try ets:last(?TABLE) of
	'$end_of_table' ->
	    {error, noproc};
        Size ->
	    Pid = ets:lookup_element(?TABLE, erlang:phash(Key, Size), 2),
            {ok, Pid}
    catch error:badarg ->
	    erlang:exit({noproc, {solo_reg, get_member, [Key]}})
    end.

%% @doc
%% Register the calling process for a portion of the keyspace.
%% @end
-spec register() -> ok.
register() ->
    ok = gen_server:call(?SERVER, {register, self()}).

%% @doc 
%% Start the solo_reg server as a named process.
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TABLE, [ordered_set,
                     named_table,
                     protected,
                     {keypos, 1},
                     {read_concurrency, true}, 
                     {write_concurrency, false}]),
    {ok, #state{}}.

handle_call({register, Pid}, _From, State) ->
    erlang:link(Pid),
    case ets:last(?TABLE) of
        '$end_of_table' ->
            ets:insert(?TABLE, {1, Pid});
        N ->
            ets:insert(?TABLE, {N + 1, Pid})
    end,
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {stop, no_cast_clause, State}.

handle_info(_Msg, State) ->
    {stop, no_info_clause, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
