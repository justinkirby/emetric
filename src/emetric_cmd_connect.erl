-module(emetric_cmd_connect).

-behaviour(emetric_command).

-export([
         command_help/0,
         deps/0,
         run/0
        ]).



command_help() ->
    {"connect","","Connect to node and respond with result"}.


deps() -> [].

run() ->
    %% if there is a cookie, change ours
    Cookie = list_to_atom(emetric_config:get_global(cookie)),
    Node = list_to_atom(emetric_config:get_global(node)),
    ok = ping(Node, Cookie).




ping(Node, Cookie) ->

    %% I need to figure out a better way to do this? yet another
    %% command line param?

    %%escript doesn't start this
    {ok,_Pid} = net_kernel:start(['emetric@localhost', shortnames]),

    %% it is possible to have the names out of sync in epmd
    %% need to wait for the names to get worked out
    global:sync(),

    erlang:set_cookie(node(), Cookie),
    case net_adm:ping(Node) of
        pong -> ok;
        pang ->
            io:format("pang~n",[]),
            halt(1)
    end.


