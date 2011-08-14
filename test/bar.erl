-module(bar).

-export([canhas/0, canhas/1]).
-export([doit/1]).

canhas() ->
    true.

canhas(A) ->
    A.

doit(A) ->
    {doit, A}.
