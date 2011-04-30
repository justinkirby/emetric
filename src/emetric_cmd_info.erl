-module(emetric_cmd_info).

-behaviour(emetric_command).

-export([
         command_help/0,
         deps/0,
         run/0
        ]).


command_help()->
    {"info",[],"Display the status and/or result of executed commands"}.

deps() -> [].

run() ->
    Injected = emetric_config:get_global(injected),
    AllMods = emetric_config:get_modules(),
    io:format("injected: ~p~n",[Injected]),
    io:format("all: ~p~n",[AllMods]).
