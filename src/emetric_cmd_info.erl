%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(emetric_cmd_info).

-behaviour(emetric_command).

-export([command_help/0,
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
    
    
