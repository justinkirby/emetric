%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2010,
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2010 by  <>
%%%-------------------------------------------------------------------
-module(emetric_log_file).

-behaviour(gen_server).
-behaviour(emetric_loadable).

-include("emetric.hrl").
%% API
-export([
         start_link/0,
         deps/0,
         sup/0,
         tick/1,
         config/1,
         reopen/0
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          run = false,
          active_file = undefined,
          active_path = undefined,
          out_dir = undefined,
          base_name = undefined,
          old_dir = undefined,
          file = 0,
          header=false, %% whether we have recorded the header to the file
          filter=emetric_filter_csv
         }).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE, worker).
tick(Acc) ->
    gen_server:cast(?SERVER, {tick, Acc}).

config(Env) -> ?CONSOLE("LOG config ~p~n",[Env]),gen_server:call(?SERVER, {config, Env}).

reopen() -> gen_server:cast(?SERVER, reopen).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    emetric_hooks:add(config_hook, fun emetric_log_file:config/1, 1),
    emetric_hooks:add(reopen_log_hook, fun emetric_log_file:reopen/0, 1),
    emetric_hooks:add(scatter_hooks, fun emetric_log_file:tick/1, 1),
    {ok, start_state(env_to_state(emetric_appsrv:config(),#state{}))}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({config, Env}, _From, State) ->
    ?CONSOLE("config hook ~p~n",[Env]),
    end_state(State),
    NewState = env_to_state(Env,State),
    {reply, ok, start_state(NewState)};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({tick, Acc}, #state{run = true} = State) ->
    {Lines, NewState} = filter_tick(Acc, State),
    io:format(NewState#state.active_file, Lines,[]),
    {noreply, NewState};
handle_cast({tick, _Acc}, State) ->
    {noreply, State};

handle_cast(reopen, State) ->
    end_state(State),
    {noreply, start_state(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
filter_tick(Acc, State) ->
    Mod = State#state.filter,
    {Header, NewState} = case State#state.header of
                            false ->
                                Head = Mod:header(Acc),
                                H = lists:flatten(io_lib:format("~s~n",[Head])),
                                NS = State#state{header = true},
                                {H, NS};
                            true ->
                                {"", State}
                        end,
    Row = Mod:row(Acc),
    {lists:flatten(io_lib:format("~s~s~n",[Header, Row])),
     NewState}.

env_to_state(Env, State) ->
    case proplists:get_value(scatter_file, Env) of
        undefined -> State;
        Config ->
            State#state{ out_dir = proplists:get_value(out_dir, Config),
                         base_name = proplists:get_value(base_name, Config),
                         old_dir = proplists:get_value(old_dir, Config)
                         }
    end.

end_state(State) ->
    case State#state.active_file of
        undefined -> ok;
        Fd ->
            ok = file:close(Fd),

            Now = emetric_util:datetime_stamp(calendar:now_to_universal_time(erlang:now())),
            OldName = lists:flatten(io_lib:format("~s.~s",
                                                  [filename:basename(State#state.active_path),
                                                   Now])),

            OldPath = filename:join([State#state.old_dir,OldName]),
            ok = filelib:ensure_dir(OldPath),

            file:rename(State#state.active_path, OldPath)
    end,
    State.

start_state(State) ->
    Filter = State#state.filter,
    FileName = lists:flatten(io_lib:format("~s_~s.~s",[State#state.base_name,
                                                       atom_to_list(node()),
                                                       Filter:type()])),
    ActivePath = filename:join([State#state.out_dir,FileName]),
                              
    ok = filelib:ensure_dir(ActivePath),

    {ok, Fd} = file:open(ActivePath, [write]),

    State#state{ run = true,
                 active_file = Fd,
                 active_path = ActivePath,
                 header = false }.
            
             
