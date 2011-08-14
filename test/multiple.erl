-module(multiple).

-include("delegate.hrl").

-delegates([{foo, [doit/0]}, bar]).
