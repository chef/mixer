%% Copyright (c) 2011 Opscode Inc.
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(delegate).

-define(PARAMS, [{0, ""},
                 {1, "A"},
                 {2, "A,B"},
                 {3, "A,B,C"},
                 {4, "A,B,C,D"},
                 {5, "A,B,C,D,E"}]).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    set_file_name(Forms),
    {EOF, Forms1} = strip_eof(Forms),
    case parse_and_expand_delegates(Forms1, []) of
        [] ->
            Forms;
        Delegates ->
            insure_no_dupes(Delegates),
            {EOF1, Forms2} = insert_stubs(Delegates, EOF, Forms1),
            finalize(Delegates, EOF1, Forms2)
    end.


%% Internal functions
set_file_name([{attribute, _, file, {FileName, _}}|_]) ->
    erlang:put(wm_delegate_file, FileName).

get_file_name() ->
    erlang:get(wm_delegate_file).

finalize(Delegates, NewEOF, Forms) ->
    insert_exports(Delegates, Forms, false, []) ++ [{eof, NewEOF}].

insert_exports(_Delegates, [], _Filter, Accum) ->
    lists:reverse(Accum);
insert_exports(Delegates, [{attribute, LineNo, delegates, _}|T], false, Accum) ->
    Attr = {attribute, LineNo, export, [{Name, Arity} || {_Mod, Name, Arity} <-
                                                             lists:flatten(Delegates)]},
    insert_exports(Delegates, T, true, [Attr|Accum]);
insert_exports(Delegates, [{attribute, _, delegates, _}|T], true, Accum) ->
    insert_exports(Delegates, T, true, Accum);
insert_exports(Delegates, [H|T], Filter, Accum) ->
    insert_exports(Delegates, T, Filter, [H|Accum]).

strip_eof(Forms) ->
    {eof, EOF} = hd(lists:reverse(Forms)),
    {EOF, lists:delete({eof, EOF}, Forms)}.

parse_and_expand_delegates([], Accum) ->
    lists:reverse(Accum);
parse_and_expand_delegates([{attribute, _, delegates, Delegates0}|T], Accum) ->
    Delegates = [expand_delegate(Delegate) || Delegate <- Delegates0],
    parse_and_expand_delegates(T, Accum ++ Delegates);
parse_and_expand_delegates([_|T], Accum) ->
    parse_and_expand_delegates(T, Accum).

expand_delegate(Name) when is_atom(Name) ->
    case catch Name:module_info(exports) of
        {'EXIT', _} ->
            io:format("~s: Unable to resolve imported module ~p~n", [get_file_name(), Name]),
            exit({error, undef_module});
        Exports ->
            [{Name, Fun, Arity} || {Fun, Arity} <- Exports, Fun /= module_info]
    end;
expand_delegate({Name, Funs}) when is_atom(Name),
                                   is_list(Funs) ->
    [{Name, Fun, Arity} || {Fun, Arity} <- Funs].

insure_no_dupes([]) ->
    ok;
insure_no_dupes([H|T]) ->
    insure_no_dupes(H, T),
    insure_no_dupes(T).

insure_no_dupes(_, []) ->
    ok;
insure_no_dupes([], _) ->
    ok;
insure_no_dupes([{Mod, Fun, Arity}|T], Rest) ->
    case find_dupe(Fun, Arity, Rest) of
        {ok, {Mod1, Fun, Arity}} ->
            io:format("~s: Importing ~p/~p from ~p and ~p~n", [get_file_name(), Fun, Arity, Mod, Mod1]),
            exit({error, duplicate_imports});
        not_found ->
            insure_no_dupes(T, Rest)
    end.

find_dupe(_Fun, _Arity, []) ->
    not_found;
find_dupe(Fun, Arity, [{Name, Fun, Arity}|_]) ->
    {ok, {Name, Fun, Arity}};
find_dupe(Fun, Arity, [H|T]) when is_list(H) ->
    case find_dupe(Fun, Arity, H) of
        not_found ->
            find_dupe(Fun, Arity, T);
        Dupe ->
            Dupe
    end;
find_dupe(Fun, Arity, [_|T]) ->
    find_dupe(Fun, Arity, T).

insert_stubs([], EOF, Forms) ->
    {EOF, Forms};
insert_stubs(Delegates, EOF, Forms) ->
    F = fun({Mod, Fun, Arity}, {CurrEOF, Acc}) ->
                {CurrEOF + 1, [generate_stub(atom_to_list(Mod), atom_to_list(Fun), Arity, CurrEOF)|Acc]} end,
    {EOF1, Stubs} = lists:foldr(F, {EOF, []}, lists:flatten(Delegates)),
    {EOF1, Forms ++ lists:reverse(Stubs)}.


generate_stub(Delegate, Name, Arity, CurrEOF) when Arity =< 5 ->
    ArgList = "(" ++ proplists:get_value(Arity, ?PARAMS) ++ ")",
    Code = Name ++ ArgList ++ "-> " ++ Delegate ++ ":" ++ Name ++ ArgList ++ ".",
    {ok, Tokens, _} = erl_scan:string(Code),
    {ok, Form} = erl_parse:parse_form(Tokens),
    replace_stub_linenum(CurrEOF, Form).

replace_stub_linenum(CurrEOF, {function, _, Name, Arity, Body}) ->
    {function, CurrEOF, Name, Arity, replace_stub_linenum(CurrEOF, Body, [])}.

replace_stub_linenum(CurrEOF, [{clause, _, Vars, [], Call}], _) ->
    [{clause, CurrEOF, replace_stub_linenum(CurrEOF, Vars, []), [], replace_stub_linenum(CurrEOF, Call, [])}];
replace_stub_linenum(_CurrEOF, [], Accum) ->
    lists:reverse(Accum);
replace_stub_linenum(CurrEOF, [{var, _, Var}|T], Accum) ->
    replace_stub_linenum(CurrEOF, T, [{var, CurrEOF, Var}|Accum]);
replace_stub_linenum(CurrEOF, [{call, _, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}], _Accum) ->
    [{call, CurrEOF, {remote, CurrEOF, {atom, CurrEOF, Mod}, {atom, CurrEOF, Fun}},
      replace_stub_linenum(CurrEOF, Args, [])}].
