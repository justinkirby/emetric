-module(emetric_cmd_inject).

-export([command_help/0,
	 deps/0,
	 run/0
	 ]).


command_help()->
    {"inject","mods=mod1,modN","Inject modules and their deps into the remote node."}.

deps() -> [emetric_cmd_connect].

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
    List = [list_to_atom("emetric_"++M) || M <- string:tokens(emetric_config:get_global(mods),",")],
    lists:foreach(fun(M) ->
			  assert_loaded(Node, M)
		  end, List),
    ok.
    

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
