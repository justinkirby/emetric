%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2011 by  <>
%%%-------------------------------------------------------------------
-module(emetric_scatter).

-behaviour(emetric_loadable).


-export([
         deps/0,
         sup/0
         ]).

-export([
         add_handler/1,
         notify/1
         ]).

-define(SERVER, ?MODULE).

-include("emetric.hrl").


deps() -> [].
sup() ->
    {?MODULE,
     {gen_event, start_link, [{local, ?MODULE}]},
     permanent, 5000, worker, [gen_event]}.



add_handler(Module) ->
    gen_event:add_handler(?SERVER, Module, []).

notify(Event) ->
    gen_event:notify(?SERVER, Event).

