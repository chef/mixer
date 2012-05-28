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
-module(mixer).

-export([parse_transform/2]).

-record(mixin, {line,
                mod,
                fname,
                alias,
                arity}).

parse_transform(Forms, _Options) ->
    set_file_name(Forms),
    {EOF, Forms1} = strip_eof(Forms),
    case parse_and_expand_mixins(Forms1, []) of
        [] ->
            Forms;
        Mixins ->
            no_dupes(Mixins),
            {EOF1, Forms2} = insert_stubs(Mixins, EOF, Forms1),
            finalize(Mixins, EOF1, Forms2)
    end.

%% Internal functions
set_file_name([{attribute, _, file, {FileName, _}}|_]) ->
    erlang:put(wm_delegate_file, FileName).

get_file_name() ->
    erlang:get(wm_delegate_file).

finalize(Mixins, NewEOF, Forms) ->
    insert_exports(Mixins, Forms, []) ++ [{eof, NewEOF}].

insert_exports([], Forms, Accum) ->
    Accum ++ Forms;
insert_exports([#mixin{line=Line}|_]=Mixins, [{attribute, Line, mixin, _}|FT], Accum) ->
    {Exports, Mixins1} = make_export_statement(Line, Mixins),
    insert_exports(Mixins1, FT, Accum ++ Exports);
insert_exports(Mixins, [H|T], Accum) ->
    insert_exports(Mixins, T, Accum ++ [H]).

strip_eof(Forms) ->
    strip_eof(Forms, []).

strip_eof([], Accum) ->
    lists:reverse(Accum);
strip_eof([{eof, EOF}|T], Accum) ->
    {EOF, lists:reverse(Accum) ++ T};
strip_eof([H|T], Accum) ->
    strip_eof(T, [H|Accum]).

parse_and_expand_mixins([], Accum) ->
    lists:keysort(2, Accum);
parse_and_expand_mixins([{attribute, Line, mixin, {Name, {Fun, Arity}}}|T], Accum) ->
    parse_and_expand_mixins(T, [#mixin{line=Line, mod=Name, alias=Fun, fname=Fun, arity=Arity}|Accum]);
parse_and_expand_mixins([{attribute, Line, mixin, {Name, {Fun, Arity}, Alias}}|T], Accum) ->
    parse_and_expand_mixins(T, [#mixin{line=Line, mod=Name, alias=Alias, fname=Fun, arity=Arity}|Accum]);
parse_and_expand_mixins([{attribute, Line, mixin, Mixins0}|T], Accum) when is_list(Mixins0) ->
    Mixins = [expand_mixin(Line, Mixin) || Mixin <- Mixins0],
    parse_and_expand_mixins(T, lists:flatten([Accum, Mixins]));
parse_and_expand_mixins([_|T], Accum) ->
    parse_and_expand_mixins(T, Accum).

expand_mixin(Line, Name) when is_atom(Name) ->
    case catch Name:module_info(exports) of
        {'EXIT', _} ->
            io:format("~s:~p Unable to resolve imported module ~p~n", [get_file_name(), Line, Name]),
            exit({error, undef_module});
        Exports ->
            [#mixin{line=Line, mod=Name, fname=Fun, alias=Fun, arity=Arity} || {Fun, Arity} <- Exports,
                                                                               Fun /= module_info]
    end;
expand_mixin(Line, {Name, Funs}) when is_atom(Name),
                                      is_list(Funs) ->
    [begin
         {Fun, Arity, Alias} = parse_mixin_ref(MixinRef),
         #mixin{line=Line, mod=Name, fname=Fun, arity=Arity, alias=Alias}
     end || MixinRef  <- Funs].

parse_mixin_ref({{Fun, Arity}, Alias}) ->
    {Fun, Arity, Alias};
parse_mixin_ref({Fun, Arity}) ->
    {Fun, Arity, Fun}.

no_dupes([]) ->
    ok;
no_dupes([H|T]) ->
    no_dupes(H, T),
    no_dupes(T).

no_dupes(_, []) ->
    ok;
no_dupes([], _) ->
    ok;
no_dupes(#mixin{mod=Mod, fname=Fun, arity=Arity, line=Line}, Rest) ->
    case find_dupe(Fun, Arity, Rest) of
        {ok, {Mod1, Fun, Arity}} ->
            io:format("~s:~p Duplicate mixin detected importing ~p/~p from ~p and ~p~n",
                      [get_file_name(), Line, Fun, Arity, Mod, Mod1]),
            exit({error, duplicate_mixins});
        not_found ->
            ok
    end.

find_dupe(_Fun, _Arity, []) ->
    not_found;
find_dupe(Fun, Arity, [#mixin{mod=Name, fname=Fun, arity=Arity}|_]) ->
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
insert_stubs(Mixins, EOF, Forms) ->
    F = fun(#mixin{mod=Mod, fname=Fun, arity=Arity, alias=Alias}, {CurrEOF, Acc}) ->
                {CurrEOF + 1, [generate_stub(atom_to_list(Mod), atom_to_list(Alias), atom_to_list(Fun), Arity, CurrEOF)|Acc]} end,
    {EOF1, Stubs} = lists:foldr(F, {EOF, []}, Mixins),
    {EOF1, Forms ++ lists:reverse(Stubs)}.


generate_stub(Mixin, Alias, Name, Arity, CurrEOF) when Arity =< 5 ->
    ArgList = "(" ++ make_param_list(Arity) ++ ")",
    Code = Alias ++ ArgList ++ "-> " ++ Mixin ++ ":" ++ Name ++ ArgList ++ ".",
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

%% Use single upper-case letters for params
make_param_list(0) ->
    "";
make_param_list(Arity) when Arity =< 26 ->
    make_param_list(Arity, []).

make_param_list(1, Accum) ->
    push_param(1, Accum);
make_param_list(Count, Accum) ->
    make_param_list(Count - 1, push_param(Count, Accum)).

push_param(Pos, []) ->
    [(64 + Pos)];
push_param(Pos, Accum) ->
    [(64 + Pos), 44|Accum].

make_export_statement(Line, Mixins) ->
    F = fun(Mixin) -> Mixin#mixin.line == Line end,
    case lists:partition(F, Mixins) of
        {[], Mixins} ->
            {[], Mixins};
        {ME, Mixins1} ->
            Export = {attribute, Line, export, [{Alias, Arity} || #mixin{alias=Alias, arity=Arity} <- ME]},
            {[Export], Mixins1}
    end.
