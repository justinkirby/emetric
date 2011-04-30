-module(emetric_loadable).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{deps, 0},
     {sup, 0}
    ];
behaviour_info(_Other) ->
    undefined.
