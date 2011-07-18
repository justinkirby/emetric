%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011,
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2011 by  <>
%%%-------------------------------------------------------------------
-module(emetric_log_flat).

-behaviour(gen_event).


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("emetric.hrl").

-record(state, {
          out_dir = ?DEFAULT_OUTDIR,
          base_name = "emetric-",
          filter = ?DEFAULT_FILTER,

          run = false,
          header=false, %% whether we have recorded the header to the file

          active_file = undefined,
          active_path = undefined,
          tick = []
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
    State = start_state(env_to_state(emetric_appsrv:config(), #state{})),
    {ok, State}.

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
handle_event(tick_start, State) -> {ok, State};
handle_event(tick_end, State) ->
    {ok, write_tick(State)};

handle_event({ejd, Data}, State) ->
    {ok, accum_data({ejd,Data},State)};

handle_event({sys, Data}, State) ->
    {ok, accum_data({sys,Data},State)};
handle_event({erls, Data}, State) ->
    {ok, accum_data({erls, Data}, State)};
handle_event({erlm, Data}, State) ->
    {ok, accum_data({erlm, Data}, State)};
handle_event({os, Data}, State) ->
    {ok, accum_data({os, Data}, State)};
handle_event({mnesia, Data}, State) ->
    {ok, accum_data({mnesia, Data}, State)};

handle_event(reopen_log_hook, State) ->
    Close = end_state(State),
    {ok, start_state(Close)};

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

accum_data({Key,Value}, #state{ tick = Tick } = State) ->
    State#state{ tick = [{Key,Value}|Tick] }.

write_tick(#state{ active_file = File } = State) ->
    {Lines, NewState} = filter_tick(State),
    io:format(File, Lines,[]),
    NewState#state{ tick = [] }.

filter_tick(#state{ tick = Tick } = State) ->
    Mod = State#state.filter,
    {Header, NewState} = case State#state.header of
                            false ->
                                Head = Mod:header(Tick),
                                H = lists:flatten(io_lib:format("~s~n",[Head])),
                                NS = State#state{header = true},
                                {H, NS};
                            true ->
                                {"", State}
                        end,
    Row = Mod:row(Tick),
    {lists:flatten(io_lib:format("~s~s~n",[Header, Row])),
     NewState}.

env_to_state(Env, #state{ out_dir = OutDirDefault,
                          base_name = BaseNameDefault,
                          filter = FilterDefault
                          } = State) ->

    case proplists:get_value(log_flat, Env) of
        undefined -> State;
        Config ->
            State#state{
              out_dir = proplists:get_value(out_dir, Config, OutDirDefault),
              base_name = proplists:get_value(base_name, Config, BaseNameDefault),
              filter = proplists:get_value(filter, Config, FilterDefault)
             }
    end.

end_state(State) ->
    case State#state.active_file of
        undefined -> ok;
        Fd -> ok = file:close(Fd)
    end,
    State#state{ run = false,
                 active_file = undefined
               }.

start_state(State) ->
    Filter = State#state.filter,
    FileName = lists:flatten(io_lib:format("~s~s.~s",[State#state.base_name,
                                                       atom_to_list(node()),
                                                       Filter:type()])),
    ActivePath = filename:join([State#state.out_dir,FileName]),

    ok = filelib:ensure_dir(ActivePath),

    %% if the file exists, then rename to datestamp.old for logrotate
    %% to pick up.
    emetric_util:archive_file(ActivePath),

    {ok, Fd} = file:open(ActivePath, [write]),

    State#state{ run = true,
                 active_file = Fd,
                 active_path = ActivePath,
                 header = false }.


