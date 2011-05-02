-module(emetric_cmd_start).

-behaviour(emetric_command).

-export([
         command_help/0,
         deps/0,
         run/0
        ]).


command_help() ->
    {"start","skip=true|false","Start the metric ticker on --node. "}.

deps() ->
    case emetric_config:get_global(skip,"false") of
        "true" ->
            [];
        "false" ->
            [emetric_cmd_inject]
    end.

run() ->
    Node = list_to_atom(emetric_config:get_global(node)),
    ok = supervise(Node),
    start_app(Node),
    ok.


supervise(Node) ->

    Env = application:get_all_env(emetric),
    

    %% get a list of base modules that can be supervised.
    %% we will have race conditions if we don't stage the start
    Supers = lists:filter(fun(M) ->
                                  emetric_util:mod_is_supervisable(M)
                          end, emetric_config:get_global(base_mods)),
    %% we need to resolve dependencies
    Ordered = lists:reverse(resolve_load_order(Supers,[])),
    %% now that we have a list of mods, get their specs
    Specs = lists:foldl(fun(M, Acc) ->
                                case M:sup() of
                                    [] -> Acc;
                                    S -> [S|Acc]
                                end end,[], Ordered),


    %% tell the injected app to start. This is done because rpc spawns
    %% a process that exits. If start_link is called then we end up
    %% with a killed supervisor/gen_server.  for convenience and
    %% sanity push the env from application over to the remote node.
    emetric_util:rpc_ok_pid(Node, emetric_appsrv, start,[Env],
                            fun(Error) ->
                                    io:format("ERROR: failed to start: ~p~n",[Error]),
                                    halt(1)
                            end,
                            fun(_Pid) -> ok end),
    emetric_util:rpc_ok(Node, emetric_appsrv, run,[Specs],
                        fun(Error) ->
                                io:format("ERROR: failed to run, ~p~n",[Error]),
                                halt(1)
                        end,
                        fun() -> io:format("running~n",[]) end),
    emetric_util:rpc_ok(Node, emetric_appsrv, config, [Env],
                        fun(Error) ->
                                io:format("ERROR: failed to config: ~p~n",[Error]),
                                halt(1)
                        end,
                        fun() -> ok end),
    ok.

start_app(Node) ->
    Gather = supervisable_mods(emetric_config:get_global(gather)),
    Scatter = supervisable_mods(emetric_config:get_global(scatter)),

    OnError = fun(Error) ->
                      io:format("ERROR: adding child, ~p~n",[Error])
              end,
    OnOk = fun(_Pid) -> ok end,

    lists:foreach(fun(Mod) ->
                          emetric_util:rpc_ok_pid(Node, supervisor, start_child,[emetric_sup, Mod:sup()],
                                                  OnError, OnOk)
                  end, lists:append(Gather, Scatter)),
    ok.






resolve_load_order([], Acc) -> Acc;
resolve_load_order([Mod|Rest], Acc) ->
    case lists:member(Mod, Acc) of
        true -> resolve_load_order(Rest, Acc);
        false ->
            case Mod:deps() of
                [] -> resolve_load_order(Rest,[Mod|Acc]);
                Deps ->
                    NewAcc = resolve_load_order(Deps, Acc)++[Mod],
                    resolve_load_order(Rest, NewAcc)
            end
    end.


supervisable_mods(L) ->
    lists:filter(fun(M) ->
                         emetric_util:mod_is_supervisable(M)
                 end, L).
