-module(single_import_test).

-include_lib("eunit/include/eunit.hrl").

-define(EXPORTS(Mod), Mod:module_info(exports)).

setup() ->
    code:add_patha(".eunit").

cleanup(_) ->
    code:del_path(".eunit").

single_test_() ->
    [{foreach,
      fun setup/0,
      fun cleanup/1,
      [{"All functions stubbed",
        fun() ->
                ?assert(lists:member({doit, 0}, ?EXPORTS(single))),
                ?assert(lists:member({doit, 1}, ?EXPORTS(single))),
                ?assert(lists:member({doit, 2}, ?EXPORTS(single)))
        end},
      {"All stubbed functions work",
       fun() ->
               ?assertMatch(doit, single:doit()),
               ?assertMatch([doit, 1], single:doit(1)),
               ?assertMatch([doit, 1, 2], single:doit(1, 2))
       end}]}].

multiple_test_() ->
    [{foreach,
      fun setup/0,
      fun cleanup/1,
      [{"All functions stubbed",
        fun() ->
                ?assert(lists:member({doit, 0}, ?EXPORTS(multiple))),
                ?assert(lists:member({doit, 1}, ?EXPORTS(multiple))),
                ?assert(lists:member({canhas, 0}, ?EXPORTS(multiple))),
                ?assert(lists:member({canhas, 1}, ?EXPORTS(multiple)))
        end},
       {"All stubbed functions work",
        fun() ->
                ?assertMatch(doit, multiple:doit()),
                ?assertMatch({doit, one}, multiple:doit(one)),
                ?assert(multiple:canhas()),
                ?assertMatch(cheezburger, multiple:canhas(cheezburger))
        end}]}].
