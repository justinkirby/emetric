-module(emetric_out_csv).


-export([header/1,
	 row/1,
	 iso_8601_fmt/1
	 ]).


header(Tick) ->
    string:join(lists:reverse(header(Tick,[])),",").

row(Tick) ->
    string:join(lists:reverse(row(Tick,[])),",").


 
header([],Acc) -> Acc;
header([{Type,Stats}|Tick],Acc) ->
    Pre = atom_to_list(Type),
    NewAcc = Acc ++ header_stat(Stats,Pre,[]),
    header(Tick,NewAcc).

header_stat([],Pre,Acc) -> Acc;
header_stat({Name,[{Key,Val}|Rest]},Pre,Acc) ->
    NewPre = new_pre(Pre,Name),
    header_stat([{Key,Val}|Rest],NewPre,Acc);
header_stat([{Name,Val}|Rest],Pre,Acc) ->
    NP = new_pre(Pre,Name),
    header_stat(Rest,Pre,header_stat({Name,Val},Pre,Acc));
header_stat({Name,Val},Pre,Acc) ->
    El = new_pre(Pre,Name),
    [El|Acc].

new_pre(Pre,Name) when is_list(Name)->
   lists:flatten(io_lib:format("~s_~s",[Pre,Name]));
new_pre(Pre,Name) ->
    lists:flatten(io_lib:format("~s_~p",[Pre,Name])).

    

row([],Acc) -> Acc;
row([{Type,Stats}|Tick],Acc) ->
    row(Tick, Acc ++ row_stat(Stats,[])).

row_stat([],Acc) -> Acc;
row_stat({Name,[{Key,Val}|Rest]},Acc) ->
    row_stat([{Key,Val}|Rest],Acc);
row_stat([{Name,Val}|Rest],Acc) ->
    row_stat(Rest,row_stat({Name,Val},Acc));
row_stat({Key,Val},Acc) ->
    NVal = lists:flatten(io_lib:format("~p",[Val])),
    [NVal|Acc].
    



iso_8601_fmt({{Y,M,D},{H,Mi,S}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		  [Y, M, D, H, Mi, S])).
