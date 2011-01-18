%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------


-module(emetric_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("emetric.hrl").
%% ===================================================================
%% API functions
%% ===================================================================

start_link(Children) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Children]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Children]) ->
    {ok, { {one_for_one, 5, 10}, Children} }.

