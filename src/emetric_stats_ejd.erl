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
-export([
         start_link/0,
         deps/0,
         sup/0,
         tick/2,
         state/1,
         state/0,
         on_receive_packet/4,
         on_send_packet/3
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
          in_ejd = false,
          users_registered = [],
          stanza_in=[],
          stanza_out=[]
         }).

%%%===================================================================
%%% API
%%%===================================================================
deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE, worker).

tick(test,[]) ->
    on_tick(0,[],#state{});
tick(Tick, Acc) ->
    gen_server:call(?SERVER, {tick, Tick, Acc}).

state(pretty) ->
    case whereis(?SERVER) of
        undefined -> "not running";
        Pid -> gen_server:call(Pid, pretty)
    end.
state() ->
    case whereis(?SERVER) of
        undefined -> "not running";
        Pid -> gen_server:call(Pid, state)
    end.



on_receive_packet(Jid, From, To, Packet) ->
    case whereis(?SERVER) of
        undefined -> ok;%% if we aren't running, not much that can be done
        Pid -> gen_server:cast(Pid, {recv, Jid, From, To, Packet})
    end.

on_send_packet(From, To, Packet) ->
    case whereis(?SERVER) of
        undefined -> ok;%% if we aren't running, not much that can be done
        Pid -> gen_server:cast(Pid, {send, From, To, Packet})
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
init([]) ->

    case code:is_loaded(ejabberd_hooks) of
        false -> ignore;  %% if there is no ejabberd, then there is nothing for us to do
        _ ->
            State = add_hooks(),
            RegUsers = [RegUsersHost ||
                           Host <- ejabberd_config:get_global_option(hosts),
                           begin
                               Users = ejabberd_auth:get_vh_registered_users_number(Host),
                               RegUsersHost = {Host, Users},
                               true
                           end],

            {ok, State#state{users_registered=RegUsers}}

    end.

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

handle_call(_Msg, _From, #state{in_ejd = false} = State) ->
    {stop, normal, State};
handle_call({tick, Tick, Acc}, _From, State) ->
    {reply, on_tick(Tick, Acc, State), State};
handle_call(pretty, _From, State) ->
    KvSpec = "  ~s ~p~n",
    Flat = fun(P) ->
                   [lists:flatten(io_lib:format(KvSpec,[K,V])) || {K,V} <- P]
           end,
    In = lists:flatten(io_lib:format("IN~n~s~n",[Flat(State#state.stanza_in)])),
    Out = lists:flatten(io_lib:format("Out~n~s~n",[Flat(State#state.stanza_out)])),

    {reply, In++Out, State};
handle_call(state, _From, State) ->
    {reply, State, State};
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
handle_cast({recv, _Jid, _From, _To, Packet}, State) ->
    Key = packet_to_key(Packet),
    New = State#state{stanza_in = incr_key(Key, State#state.stanza_in)},
    {noreply, New};

handle_cast({send, _From, _To, Packet}, State) ->
    Key = packet_to_key(Packet),
    New = State#state{stanza_out = incr_key(Key, State#state.stanza_out)},
    {noreply, New};

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
terminate(_Reason, State) ->
    case State#state.in_ejd of
        false -> ok;
        true ->
            del_hooks()
    end.

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


add_hooks() ->
    lists:foreach(fun(Host) ->
                          ejabberd_hooks:add(user_send_packet,
                                             Host,
                                             ?MODULE,
                                             on_send_packet,
                                             100),
                          ejabberd_hooks:add(user_receive_packet,
                                             Host,
                                             ?MODULE,
                                             on_receive_packet,
                                             100)
                  end,ejabberd_config:get_global_option(hosts)),
    emetric_hooks:add(gather_hooks,
                      fun emetric_stats_ejd:tick/2,
                      2),
    #state{in_ejd = true}.

del_hooks() ->
    emetric_hooks:delete(gather_hooks,
                         fun emetric_stats_ejd:tick/2,
                         2),
    lists:foreach(fun(Host) ->
                          ejabberd_hooks:delete(user_receive_packet,
                                                Host,
                                                ?MODULE,
                                                on_receive_packet,
                                                100),
                          ejabberd_hooks:delete(user_send_packet,
                                                Host,
                                                ?MODULE,
                                                on_send_packet,
                                                100)
                  end, ejabberd_config:get_global_option(hosts)),
    ok.


on_tick(Tick, Acc, State) ->
    %% loop over all the ejabberd hosts and provide:
    %% [{"example.com",[{stat, val},...]},...]
    HostStats = [{"global", constants(State) ++ global_stats()}] ++
        lists:map(fun(Host) ->
                          Users = proplists:get_value(Host,State#state.users_registered,0),
                          {Host, stats(Host,Users)}
                  end, ejabberd_config:get_global_option(hosts)),

    Ejd = {ejd,
           [{tick, Tick},
            {hosts, HostStats}
           ]},
    Stanzas = {stanzas,
               [ {stanza_in, [S || S <- State#state.stanza_in]},
                 {stanza_out, [S || S <- State#state.stanza_out]}
               ]
              },

    [Ejd,Stanzas|Acc].

constants(_State) ->
    [{now, now()}].

global_stats() ->
    Sessions = length(ejabberd_sm:dirty_get_my_sessions_list()),
    [{sessions, Sessions}].

stats(Host, Users) ->
    Online = length(ejabberd_sm:get_vh_session_list(Host)),
    [{users_total, Users},
     {users_online, Online}].



packet_to_key({xmlelement, "presence", _, _}) -> "presence";
packet_to_key({xmlelement, "message", _, _}) -> "message";
packet_to_key({xmlelement, "iq", Ats, Children}) ->
    Type = case xml:get_attr("type", Ats) of
               {value, Value} -> Value;
               false -> "none"
           end,
    Ns = first_child_ns(Children),
    lists:flatten(io_lib:format("iq_~s_~s",[Type, Ns]));
packet_to_key(_Packet) -> undefined.


first_child_ns([{xmlelement, _, Ats, _Children}|_]) ->
    xml:get_attr_s("xmlns", Ats).


incr_key(Key, PropList) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            %% have to tell the log to reopen, cause this is a new ns
            %% and we would like it to show up in the log

            %% also please note I would like a better way to do this.
            emetric_scatter:notify(stanza_new),
            [{Key, 1}|PropList];
        Old ->
            P1 = proplists:delete(Key,PropList),
            [{Key,Old+1}|P1]
    end.



