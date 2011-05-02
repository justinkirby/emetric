-module(emetric_cmd_reopen_log).

-behaviour(emetric_command).

-export([
         command_help/0,
         deps/0,
         run/0
        ]).

-include("emetric.hrl").


command_help() ->
    {"reopen_log","","Force emetric to close the log file and reopen. For logrotate"}.


deps() -> [].

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
    emetric_util:rpc_ok(Node, emetric_hooks, run, [reopen_log_hook,[]],
                        fun(Error) ->
                                ?ERROR("Error: ~p~n",[Error]),
                                halt(1)
                        end,
                        fun() -> ?CONSOLE("new log~n",[]) end).

