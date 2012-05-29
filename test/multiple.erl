-module(multiple).

-include("mixer.hrl").

-mixin([{foo, [doit/0]}, bar]).
