%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2011 by  <>
%%%-------------------------------------------------------------------
-module(emetric_log_pid).

-behaviour(gen_event).


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("emetric.hrl").

-record(state, {
          out_dir = undefined,
          base_name = "pid",
          rate = 2,
          current_ratio = 2,
          keys = [],
          files = [],
          tick = undefined
         }).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    LogPid = proplists:get_value(log_pid, emetric_appsrv:config()),
    State = #state{
      out_dir = proplists:get_value(out_dir,LogPid),
      base_name = proplists:get_value(base_name,LogPid),
      rate = proplists:get_value(rate,LogPid),
      keys = get_top_keys()
     },

    State2 = open_files(State),


    {ok, State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(tick_start,  State) ->
    {ok, State};
handle_event(tick_end,  State) ->
    NewState = pids_write(State),
    {ok, ratio_dec_reset(NewState)};    
handle_event({pid, Pids}, #state{ current_ratio = 1 } = State) ->
    {ok, State#state{ tick = {pid, Pids} } };
handle_event(reopen_log_hook, State) ->
    Close = close_files(State),
    {ok, open_files(State)};
handle_event({Key, _}, State) ->
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    close_files(State),
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
ratio_dec_reset(#state{ rate = Rate, current_ratio = 1 } = State) ->
    State#state{ current_ratio = Rate };
ratio_dec_reset(#state{ current_ratio = Cr } = State) when Cr > 1 ->
    State#state{ current_ratio = Cr - 1};
ratio_dec_reset(State) -> State.
    


get_top_keys() ->
    case emetric_util:prop_walk([gather_pid,top,info_key],
                                emetric_appsrv:config()) of
        undefined -> [];
        Keys  -> Keys
    end.


open_files(#state{ out_dir = Dir,
                   base_name = Base,
                   keys = Keys
                   } = State) ->
    Paths = [{K, Path} ||
                K <- Keys,
                begin
                    Name = Base ++ atom_to_list(K),
                    Path = filename:join([Dir,Name]),
                    true
                end],
    lists:foreach(fun({_,P}) -> filelib:ensure_dir(P) end, Paths),

    Files = [{Key, {Path, Fd}} ||
                {Key, Path} <- Paths,
                begin
                    emetric_util:archive_file(Path),
                    {ok, Fd} = file:open(Path,[write]),
                    true
                end],

    State#state{ files = Files }.

close_files(#state{ files = Files } = State) ->
    lists:foreach(fun({_Key, {_Path, Fd}}) -> file:close(Fd) end,Files),
    State#state{ files = [] }.
    
    
    
                    
    
pids_write(#state{ files = Files,
                   tick = undefined } = State) ->
    ?DEBUG("NO write~n",[]),
    State;

pids_write(#state{ files = Files,
                   tick = {pid, Tick} } = State) ->
    ?DEBUG("write~n",[]),
    lists:foreach(fun({K, T}) ->
                          {Path, File} = proplists:get_value(K,Files),
                          pid_write(T,File)
                  end,proplists:get_value(top,Tick)),
    State#state{ tick = undefined}.
                   
pid_write(T, File) ->
    Line = "~p:~p ~p~n", %% <pid>:<value> registered_name\n

    Section = lists:map(fun({Pid, Name, Value}) ->
                                io_lib:format(Line, [Pid,Value,Name])
                        end, T) ++ io_lib:format("~n~n",[]),

    ToWrite = lists:flatten(Section),

    io:format(File,ToWrite, []).

    
    
