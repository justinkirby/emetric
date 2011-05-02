%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2010,
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2010 by  <>
%%%-------------------------------------------------------------------
-module(emetric_ticker).

-behaviour(gen_server).
-behaviour(emetric_loadable).

-include("emetric.hrl").
%% API
-export([
         start_link/0,
         start_link/1,
         deps/0,
         sup/0,
         tick/0,
         scatter/1
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
-define(TICK, 2000).

-record(state, {tick = ?TICK, timer=0, tick_count=0}).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE, worker).

tick() ->
    gen_server:cast(?MODULE, {tick}).

scatter(Ticks) ->
    gen_server:cast(?MODULE, {scatter, Ticks}).
%%    erlang:start_timer(tick_sz(?TICK, 0), self(),{tick}).

%%got this from eper prf.erl:44
%% tick_sz(Tick, Offset) ->
%%     {_, Sec, Usec} = now(),
%%     Skew = Tick div 4,
%%     Tick + Skew-((round(Sec*1000+Usec/1000)-Offset+Skew) rem Tick).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(Tick) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tick], []).


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
    {ok, Tref} = start_timer(?TICK),
    {ok, #state{timer=Tref}};
init([Tick]) ->
    {ok, Tref} = start_timer(Tick),
    {ok, #state{tick=Tick, timer=Tref}}.

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
handle_cast({tick}, State) ->
    Cnt = State#state.tick_count,
    Ticks = emetric_hooks:run_fold(gather_hooks,[], Cnt),
    emetric_ticker:scatter(Ticks),
    {noreply, State#state{tick_count = Cnt+1}};
handle_cast({scatter, Ticks}, State) ->
    emetric_hooks:run(scatter_hooks, [Ticks]),
    {noreply, State};

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
start_timer(Tick) ->
    timer:apply_interval(Tick, emetric_ticker, tick,[]).
