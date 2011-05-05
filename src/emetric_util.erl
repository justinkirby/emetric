-module(emetric_util).


-export([
         mod_is_supervisable/1,
         mod_has_fun/2,
         mod_has_deps/1,
         rpc_ok_pid/6,
         rpc_ok/6,
         iso_8601_fmt/1,
         datetime_stamp/1,
         now_stamp/0,
         prop_walk/2,
         archive_file/1,
         msg/2
        ]).


mod_is_supervisable(Mod) ->
    mod_has_fun(Mod,{sup, 0}).

mod_has_deps(Mod) ->
    mod_has_fun(Mod,{deps, 0}).

mod_has_fun(Mod,{Name, Arity}) ->
    lists:member({Name, Arity}, Mod:module_info(exports)).



rpc_ok_pid(Node, Mod, Fun, Arg, OnError, OnOk) ->
    case rpc:call(Node, Mod, Fun, Arg) of
        {badrpc, Error} ->
            OnError(Error);
        Result ->
            case Result of
                {ok, Pid} ->
                    OnOk(Pid);
                ignore ->
                    OnOk(ignore);
                {error, Error} ->
                    OnError(Error)
            end
    end.


rpc_ok(Node, Mod, Fun, Arg, OnError, OnOk) ->
    case rpc:call(Node, Mod, Fun, Arg) of
        {badrpc, Error} ->
            OnError(Error);
        Result ->
            case Result of
                ok ->
                    OnOk();
                Error ->
                    OnError(Error)
            end
    end.


iso_8601_fmt({{Y, M, D},{H, Mi, S}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                                [Y, M, D, H, Mi, S])).
datetime_stamp({{Y, M, D},{H, Mi, S}}) ->
    lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",
                                [Y, M, D, H, Mi, S])).

now_stamp() ->
    emetric_util:datetime_stamp(calendar:now_to_universal_time(erlang:now())).

prop_walk([Key | Keys], Prop) ->
    case proplists:get_value(Key,Prop) of
        undefined -> undefined;
        Sub -> prop_walk(Keys,Sub)
    end;
prop_walk([],Prop) -> Prop.
    
            
    
archive_file(Path) ->
    case filelib:is_regular(Path) of
        false -> ok;
        true ->
            ToName = lists:flatten(io_lib:format("~s.~s.old",
                                                 [Path, now_stamp()])),
            file:rename(Path, ToName)
    end.

msg(Str, Args) ->
    io:format(list_to_pid("<0.0.0>"),Str,Args).
            
            
