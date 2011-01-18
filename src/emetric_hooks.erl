%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------


-module(emetric_hooks).

-behaviour(gen_server).
-behaviour(emetric_loadable).

-include("emetric.hrl").
%% API
-export([start_link/0,
	 deps/0,
	 sup/0,
	 add/3,
	 delete/3,
	 run/2,
	 run_fold/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {hooks=[]
	       }).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [].
sup() -> ?CHILD(?MODULE,worker).
    

%%--------------------------------------------------------------------
%% @doc Adds a function to the hook. Currently on gather_hooks and
%% scatter_hooks are supported.
%%
%% @spec (Hook::atom(), Function::atom(), Seq::integer()) -> ok
%% @end
%% --------------------------------------------------------------------
add(Hook,Function,Seq) when is_function(Function) ->
    gen_server:call(emetric_hooks, {add, Hook, Function,Seq}).


delete(Hook,Function,Seq) when is_function(Function) ->
    gen_server:call(emetric_hooks, {delete, Hook,Function,Seq}).
run(Hook,Args) ->
    gen_server:call(emetric_hooks, {run,Hook,Args}).
run_fold(Hook, Val, Args) ->
    gen_server:call(emetric_hooks, {run_fold, Hook, Val, Args},infinity).

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
handle_call({add, Hook, Function,Seq}, _From, State) ->
    NewHooks = lists:append(State#state.hooks,[{Hook,Seq, Function}]),
    {reply, ok, State#state{hooks=NewHooks}};

handle_call({delete, Hook, Function,Seq}, _From, State) ->
    NewHooks = lists:filter(fun({H,S,F}) ->
				    case {H,S,F} of
					{Hook, Seq,Function} -> false;
					_ -> true
				    end
			    end,State#state.hooks),
    {reply,ok, State#state{hooks=NewHooks}};

handle_call({run,Hook,Args}, _From, State) ->
    lists:foreach(fun(H) ->
			  case H of
			      %% match on the Hook called and run those
			      {Hook,Seq,Function} ->
				  Function(Args);
			      _ -> ok
			  end
		  end, State#state.hooks),
    {reply,ok,State};
handle_call({run_fold,Hook,Val,Args}, _From, State) ->
    Reply = run_fold(State#state.hooks, Hook, Val,Args),
    {reply, Reply, State};

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

run_fold([],Hook, Val, Args) -> Val;
run_fold([H|Rest],Hook, Val, Args) ->
    NewVal = case H of
		 {Hook, Seq, Function} ->
		     Function(Args,Val);
		 _ -> Val
	     end,
    case NewVal of
	stop ->
	    stopped;
	{stop,StopVal} ->
	    StopVal;
	StopVal ->
	    run_fold(Rest,Hook,NewVal, Args)
    end.
	    
	
	    
