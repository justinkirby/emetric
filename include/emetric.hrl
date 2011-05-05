                      
-define(CONSOLE(Str, Args), io:format(list_to_pid("<0.0.0>"),Str, Args)).

%%TODO: move following to a logger
-define(DEBUG(Str, Args), io:format(list_to_pid("<0.0.0>"),Str, Args)).
-define(INFO(Str, Args), io:format(list_to_pid("<0.0.0>"),Str, Args)).
-define(WARN(Str, Args), io:format(list_to_pid("<0.0.0>"),Str, Args)).
-define(ERROR(Str, Args), io:format(list_to_pid("<0.0.0>"),Str, Args)).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, brutal_kill, Type, [I]}).


-define(CONFIG_PATHS, ["/etc/emetric.cfg",os:getenv("HOME")++"/.emetric"]).


-define(DEFAULT_OUTDIR, "/tmp/emetric").
-define(DEFAULT_FILTER, emetric_filter_csv).
