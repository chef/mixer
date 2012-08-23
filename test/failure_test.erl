-module(failure_test).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EXPORTS(Mod), Mod:module_info(exports)).

duplicate_test_() ->
    [{<<"Duplicate mixins detected">>,
      fun() ->
              Error = compile_bad_test_file("duplicates"),
              ?assertMatch({error,[{"duplicates.erl",
                                    [{none,compile,
                                      {parse_transform,mixer,{error,duplicate_mixins}}}]}],
                            []}, Error) end},
     {<<"Missing mixin detected">>,
      fun() ->
              F = fun(Name0) ->
                          Name = Name0 ++ ".erl",
                          Error = compile_bad_test_file(Name0),
                          ?assertMatch({error,[{Name,
                                                [{none,compile,
                                                  {parse_transform,mixer,{error,undef_mixin_module}}}]}],
                                        []}, Error) end,
              [F(Module) || Module <- ["missing", "missing_all"]] end}].

conflicting_mixins_test_() ->
    [{<<"Conflicting mixins detected">>,
      fun()->
              Error = compile_bad_test_file("conflicts"),
              ?assertMatch({error,[{"conflicts.erl",
                                    [{none,compile,
                                      {parse_transform,mixer,{error,duplicate_mixins}}}]}],
                            []}, Error) end}].

%% Internal functions
compile_bad_test_file(Module) ->
    From = filename:join(["..", "test", Module ++ ".erl.bad"]),
    To = "./" ++ Module ++ ".erl",
    {ok, BytesCopied} = file:copy(From, To),
    {ok, Info} = file:read_file_info(From),
    ?assertEqual(Info#file_info.size, BytesCopied),
    compile:file(To, [{i, "../include"}, return_errors]).
