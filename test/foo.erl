-module(foo).

-export([doit/0, doit/1, doit/2]).

doit() ->
    doit.

doit(A) ->
    [doit, A].

doit(A, B) ->
    [doit, A, B].
