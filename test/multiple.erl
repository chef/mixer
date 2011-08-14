-module(multiple).

-include("mixin.hrl").

-mixin([{foo, [doit/0]}, bar]).
