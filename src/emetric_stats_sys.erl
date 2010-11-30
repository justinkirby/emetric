%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2010, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2010 by  <>
%%%-------------------------------------------------------------------
-module(emetric_stats_sys).

-behaviour(gen_server).

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

%% I did not know you call a fun in record init! cool
-record(state, {strategy=strategy(),
		node = node(),
		total_ram=0,
		cores=1,
		cache=[],
		now=now()
	       }).






%%%===================================================================
%%% API
%%%===================================================================q

deps() -> [emetric_hooks].
sup() -> ?CHILD(?MODULE,worker).
tick(test,[]) ->
    on_tick(0,[],init_state(#state{}));
tick(Tick,Acc) ->
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
    emetric_hooks:add(gather_hooks, fun(T,A) -> emetric_stats_sys:tick(T,A) end,1),
    {ok, init_state(#state{})}.

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
handle_call({tick, Tick,Acc}, _From, State) ->
    {reply, on_tick(Tick,Acc,State),State};
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


%% tag                  [unit]    source
%% node                 [atom()]  erlang:node()
%% now			[now()]   erlang:now()
%% procs	        [count]   erlang:system_info(process_count)
%% context_switches	[count/s] erlang:statistics(context_switches)
%% gcs	                [count/s] erlang:statistics(garbage_collection)
%% gc_reclaimed	        [byte/s]  erlang:statistics(garbage_collection)
%% io_in		[byte/s]  erlang:statistics(io)
%% io_out		[byte/s]  erlang:statistics(io)
%% reductions		[count/s] erlang:statistics(reductions)
%% reducts_since	[count/s] erlang:statistics(reductions)
%% run_queue		[count]   erlang:statistics(run_queue)
%% total		[byte]    erlang:memory()
%% processes		[byte]    erlang:memory()
%% processes_used	[byte]    erlang:memory()
%% system		[byte]    erlang:memory()
%% atom			[byte]    erlang:memory()
%% atom_used		[byte]    erlang:memory()
%% binary		[byte]    erlang:memory()
%% code			[byte]    erlang:memory()
%% ets			[byte]    erlang:memory()
%% user			[frac]    /proc/stat
%% nice			[frac]    /proc/stat
%% kernel		[frac]    /proc/stat
%% idle			[frac]    /proc/stat
%% iowait		[frac]    /proc/stat
%% ctxt                 [frac]    /proc/stat
%% beam_user,		[frac]    /proc/self/stat
%% beam_kernel,		[frac]    /proc/self/stat
%% beam_vss		[byte]    /proc/self/stat
%% beam_rss             [pages]   /proc/self/stat
%% beam_minflt          [count/s] /proc/self/stat
%% beam_majflt          [count/s] /proc/self/stat
%% total_ram            [byte]    /proc/meminfo
on_tick(Tick,Acc,State) ->
    Data = [{tick,Tick}]++constants(State)++stats()++os_info(State#state.strategy),
    Acc++[{sys,Data}].

constants(#state{node=Node,total_ram=Total_ram,cores=Cores}) ->
  [{node, Node},{total_ram,Total_ram},{cores,Cores}].


stats() ->
    Procs = erlang:system_info(process_count),
    {Ctx, 0} = erlang:statistics(context_switches),
    {GCs,GCwords,0} = erlang:statistics(garbage_collection),
    {{input,IoIn},{output,IoOut}} = erlang:statistics(io),
    {Reducts,ReductSince} = erlang:statistics(reductions),
    RunQ = erlang:statistics(run_queue),

    [{now,now()},
     {procs,Procs},
     {context_switches,Ctx},
     {gcs, GCs},
     {gc_reclaimed,GCwords},
     {io_in,IoIn},
     {io_out,IoOut},
     {reductions,Reducts},
     {reducts_since,ReductSince},
     {run_queue,RunQ} |
     erlang:memory()].
     

%% OS info
%% only the 'linux' (i.e. linux 2.6 or higher) strategy implemented
-record(fds,{proc_stat,proc_self_stat}).


os_info({linux,#fds{proc_stat=FDs,proc_self_stat=FDss}}) ->
  proc_stat(FDs)++proc_self_stat(FDss);
os_info(_) ->
  [].

proc_stat(FDs) ->
%%user nice kernel idle iowait irq softirq steal
  {ok,Str} = file:pread(FDs,0,200),
  case string:tokens(Str," \n") of
    ["cpu",User,Nice,Kernel,Idle,Iowait|_] -> ok;
    _ -> User=Nice=Kernel=Idle=Iowait=0
  end,
  lists:zip([user,nice,kernel,idle,iowait],
	    [to_sec(J) || J <- [User,Nice,Kernel,Idle,Iowait]]).

proc_self_stat(FDss) ->
%%% pid,comm,state,ppid,pgrp,session,tty_nr,tpgid,flags,
%%% minflt,cminflt,majflt,cmajflt,utime,stime,cutime,cstime,
%%% priority,nice,num_threads,itrealvalue,starttime,vsize,rss
  {ok,Str} = file:pread(FDss,0,200),
  case string:tokens(Str," ") of
    [_,_,_,_,_,_,_,_,_,
     Minflt,_,Majflt,_,Utime,Stime,_,_,
     _,_,_,_,_,Vsize,Rss|_] -> ok;
    _ -> Minflt=Majflt=Utime=Stime=Vsize=Rss=0
  end,
  lists:zip([beam_user,beam_kernel,beam_vss,beam_rss,beam_minflt,beam_majflt],
	    [to_sec(Utime),to_sec(Stime),to_int(Vsize),
	     to_int(Rss), %% in pages...
	     to_int(Minflt),to_int(Majflt)]).

to_sec(J) ->
  to_int(J)/100. %should use a better transform jiffies->secs

to_int(J) -> list_to_integer(J).

cores({linux,#fds{proc_stat=Proc_stat}}) ->
  {ok,Str} = file:pread(Proc_stat,0,1000),
  Toks = string:tokens(Str,"\n"),
  case length(lists:takewhile(fun(S)->lists:prefix("cpu",S) end,Toks)) of
    1 -> 1;
    M -> M-1
  end;
cores(_) ->
  1.

%% only doing linux now
strategy() ->
    case {os:type(),os:version()} of
	{{unix,linux},{2,_,_}} -> {linux, init_linux()};
	_ -> {none,[]}
    end.
    
init_linux() ->
    {ok,FDs} = file:open("/proc/stat",[read]),
    {ok,FDss} = file:open("/proc/self/stat",[read]),
    #fds{proc_stat=FDs, proc_self_stat=FDss}.




total_ram() ->
  case file:open("/proc/meminfo",[read]) of
    {ok,FD} ->
      try {ok,Str} = file:pread(FD,0,30),
	  ["MemTotal:",T,"kB"|_] = string:tokens(Str," \n"),
	  list_to_integer(T)*1024
      catch _:_ -> 0
      after file:close(FD)
      end;
    _ -> 0
  end.


init_state(State = #state{strategy={linux,_}}) ->
    State#state{total_ram = total_ram(),
		cores = cores(State#state.strategy)};
init_state(State) ->
    State.
