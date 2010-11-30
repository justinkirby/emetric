%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2010, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2010 by  <>
%%%-------------------------------------------------------------------
-module(emetric_stats_ejd).

-behaviour(gen_server).
-behaviour(emetric_loadable).

-include("emetric.hrl").
%% API
-export([start_link/0,
	 deps/0,
	 sup/0,
	 tick/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE,worker).

tick(test,[]) ->
    on_tick(0,[],#state{});
tick(Tick,Acc) ->
    gen_server:call(?SERVER, {tick,Tick,Acc}).


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
    emetric_hooks:add(gather_hooks, fun(T,A) -> emetric_stats_ejd:tick(T,A) end,2),
    {ok, #state{}}.

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
handle_call({tick, Tick, Acc}, _From, State) ->
    {reply,on_tick(Tick,Acc,State),State};
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

on_tick(Tick,Acc,State) ->
    %% loop over all the ejabberd hosts and provide:
    %% [{"example.com",[{stat,val},...]},...]
    HostStats = lists:map(fun(Host) ->
				  {Host, stats(Host)}
			  end,ejabberd_config:get_global_option(hosts)),
    Data = [{ejd,
	     {tick,Tick},
	     {global, constants(State) ++ global_stats()},
	     {hosts, HostStats}	     
	    }],
    Acc++Data.

constants(_State) ->
    [{now,now()}].

global_stats() ->
    Sessions = length(ejabberd_sm:dirty_get_my_sessions_list()),
    [{sessions,Sessions}].

stats(Host) ->
    Users = ejabberd_auth:get_vh_registered_users_number(Host),
    Online = length(ejabberd_sm:get_vh_session_list(Host)),
    [{users_total, Users},
     {users_online, Online}].
    
