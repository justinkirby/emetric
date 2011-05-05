-module(emetric_filter_csv).


-export([
         type/0,
         header/1,
         row/1
        ]).

type() -> "csv".

header(Tick) ->
    string:join(lists:reverse(header(Tick,[])),",").

row(Tick) ->
    string:join(lists:reverse(row(Tick,[])),",").



header([], Acc) -> Acc;
header([{Type, Stats}|Tick], Acc) when is_atom(Type) ->
    Pre = atom_to_list(Type),
    NewAcc = Acc ++ header_stat(Stats, Pre,[]),
    header(Tick, NewAcc);
header([{Type, Stats}|Tick], Acc) when is_list(Type) ->
    NewAcc = Acc ++ header_stat(Stats, Type, []),
    header(Tick, NewAcc).
    

header_stat([], _Pre, Acc) -> Acc;
header_stat({Name,[{Key, Val}|Rest]}, Pre, Acc) ->
    NewPre = new_pre(Pre, Name),
    header_stat([{Key, Val}|Rest], NewPre, Acc);
header_stat([{Name, Val}|Rest], Pre, Acc) ->
%%    NP = new_pre(Pre, Name),
    header_stat(Rest, Pre, header_stat({Name, Val}, Pre, Acc));
header_stat({Name, _Val}, Pre, Acc) ->
    El = new_pre(Pre, Name),
    [El|Acc].

new_pre(Pre, Name) when is_list(Name)->
    lists:flatten(io_lib:format("~s_~s",[Pre, Name]));
new_pre(Pre, Name) ->
    lists:flatten(io_lib:format("~s_~p",[Pre, Name])).



row([], Acc) -> Acc;
row([{_Type, Stats}|Tick], Acc) ->
    row(Tick, Acc ++ row_stat(Stats,[])).

row_stat([], Acc) -> Acc;
row_stat({_Name,[{Key, Val}|Rest]}, Acc) ->
    row_stat([{Key, Val}|Rest], Acc);
row_stat([{Name, Val}|Rest], Acc) ->
    row_stat(Rest, row_stat({Name, Val}, Acc));
row_stat({now, Val}, Acc) ->
    Time = calendar:now_to_universal_time(Val),
    NVal = lists:flatten(io_lib:format("~s",[emetric_util:iso_8601_fmt(Time)])),
    [NVal|Acc];
row_stat({_Key, []}, Acc) -> [""|Acc];
row_stat({_Key, Val}, Acc) ->
    NVal = lists:flatten(io_lib:format("~p",[Val])),
    [NVal|Acc].







