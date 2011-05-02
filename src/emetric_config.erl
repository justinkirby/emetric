-module(emetric_config).

-export([
         set_global/2,
         get_global/2,
         get_global/1,
         get_modules/0,
         config_file/0
        ]).

-include("emetric.hrl").

config_file() ->
    case get_global(noconfig) of
        1 -> io:format("noconfigs ~n",[]), ok;
        _ ->
            %% is there a file on the command line?
            Configs = case get_global(config) of
                          undefined -> ?CONFIG_PATHS;
                          Cfg -> ?CONFIG_PATHS ++ [Cfg]
                      end,
            load_config(Configs)
    end.

load_config([]) -> ok;
load_config([Config | Rest]) ->
    case filelib:is_regular(Config) of
        false -> load_config(Rest);
        true ->
            {ok, Terms} = file:consult(Config),
            lists:foreach(fun({Key, Value}) ->
                                  set_global(Key, Value)
                          end, Terms),                                  
            load_config(Rest)
    end.
    


set_global(gather, Value) ->
    set_module_list(gather, Value);
set_global(scatter, Value) ->
    set_module_list(scatter, Value);
set_global(filter, Value) ->
    set_module_list(filter, Value);
set_global(mods, Value) ->
    Mods = build_module_list("", Value),
    application:set_env(emetric, mods, Mods);
set_global(Key, Value) when is_integer(Value) ->
    application:set_env(emetric, Key, erlang:max(1, Value));
set_global(Key, Value) ->
    application:set_env(emetric, Key, Value).


get_global(Key, Default) ->
    case application:get_env(emetric, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

get_global(Key) ->
    get_global(Key, undefined).

get_modules() ->
    Keys = [base_mods, mods, gather, scatter, filter],
    get_modules(Keys,[]).

get_modules([], Acc) -> Acc;
get_modules([Key|Rest], Acc) ->
    Mods = get_global(Key,[]),
    get_modules(Rest, Acc++Mods).



set_module_list(Base, Value) ->
    Key = list_to_atom(atom_to_list(Base) ++ "_prefix"),
    Pre = get_global(Key),
    Mods = build_module_list(Pre, Value),
    application:set_env(emetric, Base, Mods).


build_module_list(Prefix, Args) ->
    build_module_list(Prefix, string:tokens(Args,","),[]).
build_module_list(_Prefix,[], Mods) ->
    Mods;
build_module_list(Prefix,[Base|Rest], Mods) ->
    Mod = list_to_atom(Prefix++Base),
    build_module_list(Prefix, Rest, Mods++[Mod]).



