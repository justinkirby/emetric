%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2010,
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2010 by  <>
%%%-------------------------------------------------------------------
-module(emetric_appsrv).

-behaviour(gen_server).
-behaviour(emetric_loadable).

%% API
-export([
         start/1,
         start_link/0,
         config/0,
         config/1,
         stop/0,
         deps/0,
         sup/0,
         run/1,
         ping/0
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

-include("emetric.hrl").

-record(state, {env, sup}).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [].
sup() -> [].
run(Specs) ->
    gen_server:call(?SERVER,{start, Specs}).
start(Env) ->
    gen_server:start({local,?SERVER}, ?MODULE, [Env],[]).

config(Env) ->
    gen_server:call(?SERVER,{config, Env}).

config() ->
    gen_server:call(?SERVER,config).

stop() ->
    gen_server:call(?SERVER, stop).

ping() ->
    case whereis(?SERVER) of
        undefined -> pang;
        Pid ->
            case gen_server:call(Pid, ping) of
                pong -> pong;
                Status -> Status
            end
    end.
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
init([Env]) ->
    {ok, #state{env = Env}}.

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
handle_call({start, Specs}, _From, State) ->
    {ok, Sup } = emetric_sup:start_link(Specs),
    {reply, ok, State#state{sup = Sup}};
handle_call({config, Env}, _From, State) ->

    emetric_hooks:run(config_hook, [Env]),
    
    %% we spawn here, otherwise this is a deadlock call when the
    %% events try to pull the config
    spawn_link(fun() -> add_scatter_events(Env) end),

    {reply, ok, State#state{env = Env}};
handle_call(config, _From, State) ->
    {reply, State#state.env, State};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(ping, _From, State) ->
    {reply, pong, State};
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
add_scatter_events(Config) ->
    %% only decent way I could find to add event to event manager. ...
    Scatters = proplists:get_value(scatter, Config),

    lists:foreach(fun(S) ->
                          emetric_scatter:add_handler(S)
                  end,Scatters).
