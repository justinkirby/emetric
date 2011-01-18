%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(emetric_cmd_inject).

-behaviour(emetric_command).

-export([command_help/0,
	 deps/0,
	 run/0
	 ]).

command_help()->
    {"inject","mods=m1,mN gather=m1,mN scatter=s1,sN","Inject modules and their deps into the remote node."}.

deps() -> [emetric_cmd_connect].

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
%%    List = [list_to_atom("emetric_"++M) || M <- string:tokens(emetric_config:get_global(mods),",")],
    List = emetric_config:get_modules(),
    Injected = inject_mod(Node,List,[]),

    emetric_config:set_global(injected,Injected),
    
    ok.

inject_mod(_Node,[],Done) -> Done;

inject_mod(Node,[Mod|Rest],Done) ->
    case lists:member(Mod, Done) of
	true -> inject_mod(Node,Rest,Done);
	false ->
	    case lists:member({deps,0},Mod:module_info(exports)) of
		%% does not have deps exported, assume it is vanilla
		%% with no deps
		false ->
		    ok = assert_loaded(Node,Mod),
		    inject_mod(Node,Rest,Done++[Mod]);
		true ->
		    ok = assert_loaded(Node,Mod),
		    Deps =  Mod:deps(),
		    inject_mod(Node,Deps++Rest,Done++[Mod])
	    end
    end.


assert_loaded(Node,Mod) ->
    case rpc:call(Node,Mod,module_info,[compile]) of
	{badrpc,{'EXIT', {undef,_}}} ->
	    netload(Node,Mod),
	    assert_loaded(Node,Mod);
	{badrpc,_} ->
	    ok;
	CompInfo when is_list(CompInfo) ->
	    case {ftime(CompInfo),ftime(Mod:module_info(compile))} of
		{interpreted,_} ->
		    ok;
		{TargT, HostT} when TargT < HostT ->
		    netload(Node, Mod),
		    assert_loaded(Node,Mod);
		_ ->
		    ok
	    end
    end.

netload(Node,Mod) ->
    {Mod, Bin,Fname} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod,Fname,Bin]).

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).
