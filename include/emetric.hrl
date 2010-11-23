-define(CONSOLE(Str, Args), io:format(Str,Args)).

%%TODO: move following to a logger
-define(DEBUG(Str, Args), io:format(Str, Args)).
-define(INFO(Str, Args), io:format(Str, Args)).
-define(WARN(Str, Args), io:format(Str, Args)).
-define(ERROR(Str, Args), io:format(Str, Args)).
