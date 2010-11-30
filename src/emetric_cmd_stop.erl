-module(emetric_cmd_stop).

-behaviour(emetric_command).

-export([command_help/0,
	 deps/0,
	 run/0
	 ]).


command_help() ->
    {"stop","skip=true|false","Shutdown the emetric app on the --node "}.

deps() ->
    case emetric_config:get_global(skip,"false") of
	"true" ->
	    [];
	"false" ->
	    [emetric_cmd_inject]
    end.

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
    rpc:call(Node,emetric_appsrv,stop,[]),
    ok.
