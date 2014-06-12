`foo.erl`:

    -module(foo).

    -export([doit/0, doit/1, doit/2]).

    doit() ->
        doit.

    doit(A) ->
        [doit, A].

    doit(A, B) ->
        [doit, A, B].

Module `bar.erl` which 'mixes in' `foo`:

    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([foo]).

or only specific functions from `foo`:

    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, [doit/0, doit/2]}]).

Another version of `bar.erl` which mixes in all functions from `foo` and select functions from `baz`:

    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([foo, {baz, [doit/0, doit/1]}]).

One more version of `bar.erl` which mixes in `foo:doit/0` and renames it to `do_it_now/0`:

    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, [{doit/0, do_it_now}]}]).

The original motivation for this parse transform was to permit reuse of functions implementing common
logic for tasks such as signature verification and authorization across multiple webmachine resources.