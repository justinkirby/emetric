%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2011 by  <>
%%%-------------------------------------------------------------------
-module(emetric_log_stanza).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("emetric.hrl").

-record(state, {
          %% configured state
          out_dir = ?DEFAULT_OUTDIR,
          base_name = "stanza",
          filter = ?DEFAULT_FILTER,

          %% default run state
          run = false,
          header = false,

          %% calculate stated
          active_file = undefined,
          active_path = "",
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
    State = env_to_state(emetric_appsrv:config(), #state{}),
    {ok, start_state(State)}.

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
handle_event({stanzas, [{stanza_in,[]},{stanza_out,[]}]}, State) ->
    {ok, State};
handle_event({stanzas, Data}, #state{ tick = T } = State) ->
    {ok, State#state{ tick = [{stanza, Data}| T] } };
handle_event(stanza_new, State) ->
    {ok, start_state(end_state(State))};
handle_event(reopen_log_hook, State) ->
    {ok, start_state(end_state(State))};
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
env_to_state(Env, #state{ out_dir = OutDirDefault,
                          base_name = BaseNameDefault,
                          filter = FilterDefault
                          } = State) ->
    case proplists:get_value(log_stanza, Env) of
        undefined -> State;
        Config ->
            State#state{
              out_dir = proplists:get_value(out_dir, Config, OutDirDefault),
              base_name = proplists:get_value(base_name, Config, BaseNameDefault),
              filter = proplists:get_value(filter, Config, FilterDefault)
             }
    end.


start_state(#state{ filter = Filter,
                    base_name = Base,
                    out_dir = OutDir} = State) ->

    ActivPath = filename:join([OutDir, Base++"."++Filter:type()]),

    ok = filelib:ensure_dir(ActivPath),

    emetric_util:archive_file(ActivPath),


    {ok, Fd} = file:open(ActivPath, [write]),


    State#state{ run = true,
                 active_file = Fd,
                 active_path = ActivPath,
                 header = false }.

end_state(#state{ active_file = Fd } = State) ->
    case Fd of
        undefined -> ok;
        Fd -> ok = file:close(Fd)
    end,

    State#state{ run = false,
                 active_file = undefined,
                 header = false }.
              
%% if we have nothing, don't log it.
write_tick(#state{ tick = [] } = State) ->
    State;
            
write_tick(#state{ active_file = Fd } = State) ->
    {Lines, NewState} = format_tick(State),
    io:format(Fd, Lines, []),
    NewState#state{ tick = [] }.

format_tick(#state{ tick = Tick,
                    filter = Filter
                  } = State) ->

    {Header, NewState} = case State#state.header of
                             false ->
                                 Head = Filter:header(Tick),
                                 H = lists:flatten(io_lib:format("~s~n", [Head])),
                                 {H, State#state{ header = true }};
                             true->
                                 {"", State}
                         end,
    
    Row = Filter:row(Tick),
    {lists:flatten(io_lib:format("~s~s~n",[Header,Row])),
     NewState}.
    
