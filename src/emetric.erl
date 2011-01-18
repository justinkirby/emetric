%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(emetric).

-export([main/1]).

main(Args) ->
    case catch(emetric_core:run(Args)) of
	ok ->
	    ok;
	{error, failed} ->
	    halt(1);
	Error ->
	    io:format("Uncaught error in emetric_core: ~p\n",[Error]),
	    halt(1)
    end.
