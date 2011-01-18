%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------


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
