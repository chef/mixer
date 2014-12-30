-module(multiple).

-include("mixer.hrl").

-mixin([{foo, [doit/0]},
        bar,
        {foo, except, [doit/0, doit/1]}]).
