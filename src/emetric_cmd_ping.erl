-module(emetric_cmd_ping).

-behaviour(emetric_command).

-export([ command_help/0,
          deps/0,
          run/0
        ]).


-define(PANG(Msg, Rc),
        io:format("pang~nERROR: ~p~n",[Msg]),
        halt(Rc)).
-define(PONG(), io:format("pong~n",[])).

command_help() ->
    {"ping","","Ping emetric running in node. Result will be pong or pang."}.


deps() -> [emetric_cmd_connect].

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
    Mod = emetric_appsrv,

    is_loaded(Node, Mod),
    is_running(Node,Mod),
    io:format("pong~n",[]).

is_loaded(Node, Mod) ->
    %% is the gen serv loaded?
    case rpc:call(Node,Mod, module_info,[compile]) of
        {badrpc,{'EXIT', {undef,_}}} ->
            ?PANG("badrpc EXIT", 2);
        {badrpc,Reason} ->
            ?PANG(Reason,2);
        _ ->
            ok
    end.

is_running(Node, Mod) ->
    case rpc:call(Node,Mod,ping,[]) of
        pong -> ok;
        pang -> ?PANG("emetric is loaded, but NOT running",3);
        R -> ?PANG(R,3)
    end.
