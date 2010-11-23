-module(emetric_config).

-export([set_global/2,
	 get_global/2,
	 get_global/1
	 ]).

-include("emetric.hrl").

	 


set_global(Key,Value) when is_integer(Value) ->
    application:set_env(emetric, Key, erlang:max(1,Value));
set_global(Key, Value) ->
    application:set_env(emetric, Key, Value).


get_global(Key,Default) ->
    case application:get_env(emetric, Key) of
	undefined ->
	    Default;
	{ok,Value} ->
	    Value
    end.

get_global(Key) ->
    get_global(Key,undefined).
    
