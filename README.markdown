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
    -include_lib("mixin/include/mixin.hrl").
    -mixin([foo]).

or only specific functions from `foo`:

    -module(bar).
    -include_lib("mixin/include/mixin.hrl").
    -mixin([{foo, [doit/0, doit/2]}]).

Another version of `bar.erl` which mixes in all functions from `foo` and select functions from `baz`:

    -module(bar).
    -include_lib("mixin/include/mixin.hrl").
    -mixin([foo, {baz, [doit/0, doit/1]}]).

From the Department of Egregious Hacks, a version of `bar.erl` which mixes in the public API of the
`lists` module:

    -module(bar).
    -include_lib("mixin/include/mixin.hrl").
    -mixin([lists]).
---
    1> bar:module_info(exports).
    [{member,2},
    {reverse,2},
    {keymember,3},
    {keysearch,3},
    {keyfind,3},
    {foreach,2},
    {foldl,3},
    {reverse,1},
    {concat,1},
    {filter,2},
    {zf,2},
    {delete,2},
    {flatten,1},
    {map,2},
    {keydelete,3},
    {keyreplace,4},
    {splitwith,2},
    {sort,1},
    {split,2},
    {dropwhile,2},
    {takewhile,2},
    {mapfoldr,3},
    {mapfoldl,3},
    {partition,2},
    {foldr,3},
    {flatmap,2},
    {any,2},
    {all,...},
    {...}|...]
    2> bar:seq(1, 10).
    [1,2,3,4,5,6,7,8,9,10]
    3>
The original motivation for this parse transform was to permit reuse of functions implementing common
logic for tasks such as signature verification and authorization across multiple webmachine resources.