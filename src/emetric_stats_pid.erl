%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2011 by  <>
%%%-------------------------------------------------------------------
-module(emetric_stats_pid).

-behaviour(gen_server).
-behaviour(emetric_loadable).

-include("emetric.hrl").

%% API
-export([
         deps/0,
         sup/0,
         tick/2,
         start_link/0,
         get_top/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {top = []}).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE, worker).


tick(Tick, Acc) ->
    gen_server:call(?SERVER, {tick, Tick, Acc}).
    
    

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
    Config = emetric_appsrv:config(),
    Gpid = proplists:get_value(gather_pid, Config),

    emetric_hooks:add(gather_hooks, fun emetric_stats_pid:tick/2, 2),

    {ok, #state{top = proplists:get_value(top, Gpid)}}.

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
handle_call({tick, Tick, Acc} , _From, State) ->
    {reply, on_tick(Tick, Acc, State), State};
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

on_tick(Tick, Acc, State) ->
    %% get the list of named pids we want and join it with the info we
    %% want from process_info
    Top = get_top(State#state.top),
    Data = {pid,
            [{tick,Tick},
             {top, Top}
            ]},
    [Data|Acc].
    

get_top(Top) ->
    TopKeys = proplists:get_value(info_key, Top),
    Max = proplists:get_value(max_count, Top),
    Pids = erlang:processes(),
    get_top(TopKeys, Pids, Max, []).
    
    
get_top([], _Pids, _Max, Acc) -> Acc;
get_top([Key | Rest], Pids, Max, Acc) ->
    Unsorted =  [{P, V} ||
                P <- Pids,
                begin
                    {Key, V} = erlang:process_info(P, Key),
                    true
                end ],
    Sorted = lists:reverse(lists:sort(fun({_Pa, Va}, {_Pb, Vb}) ->
                                              if
                                                  Va =< Vb -> true;
                                                  true -> false
                                              end end, Unsorted)),
    {Trunc, _} = lists:split(Max, Sorted),

    %%make the pid pretty, get the init func or reg name
    Pretty = [{P, Name, V} ||
                 {P,V} <- Trunc,
                 begin
                      [{registered_name, Reg},{initial_call, Initial}] =
                         erlang:process_info(P,[registered_name,initial_call]),
                     Name = case Reg of
                                [] -> Initial;
                                _ -> Reg
                            end,
                     true
                 end ],

    get_top(Rest,Pids,Max,[{Key, Pretty} | Acc]).
            
