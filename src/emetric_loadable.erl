%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------


-module(emetric_loadable).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{deps,0},
     {sup,0}
    ];
behaviour_info(_Other) ->
    undefined.
