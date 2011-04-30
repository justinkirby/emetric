!
-module(emetric_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/1
        ]).

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

