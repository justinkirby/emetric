%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------


-define(CONSOLE(Str, Args), io:format(Str,Args)).

%%TODO: move following to a logger
-define(DEBUG(Str, Args), io:format(Str, Args)).
-define(INFO(Str, Args), io:format(Str, Args)).
-define(WARN(Str, Args), io:format(Str, Args)).
-define(ERROR(Str, Args), io:format(Str, Args)).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, brutal_kill, Type, [I]}).
