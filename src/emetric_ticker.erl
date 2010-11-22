-module(emetric_ticker).
-export([tick/0
	 ]).

-define(TICK,2000).



tick() ->
    erlang:start_timer(tick_sz(?TICK,0),self(),{tick}).

%%got this from eper prf.erl:44
tick_sz(Tick,Offset) ->
    {_,Sec,Usec} = now(),
    Skew = Tick div 4,
    Tick + Skew-((round(Sec*1000+Usec/1000)-Offset+Skew) rem Tick).
