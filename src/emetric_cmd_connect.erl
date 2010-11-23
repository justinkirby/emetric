-module(emetric_cmd_connect).


-export([ command_help/0,
	  deps/0,
	  run/0
	  ]).



command_help() ->
    {"connect","","Connect to node and respond with result"}.


deps() -> [].

run() ->
    %% if there is a cookie, change ourse
    Cookie = list_to_atom(emetric_config:get_global(cookie)),
    Node = list_to_atom(emetric_config:get_global(node)),
    ok = ping(Node,Cookie).




ping(Node,Cookie) ->
    %%escript doesn't start this
    {ok,_Pid} = net_kernel:start(['emetric@localhost',shortnames]),

    %% it is possible to have the names out of sync in epmd
    %% need to wait for the names to get worked out
    global:sync(),

    erlang:set_cookie(node(),Cookie),
    pong = net_adm:ping(Node),

    ok.
    
    



    
