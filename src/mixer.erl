%% -*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
%%
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(mixer).

-export([parse_transform/2]).

-record(mixin, {line,
                mod,
                fname,
                alias,
                arity}).

-spec parse_transform([term()], [term()]) -> [term()].
parse_transform(Forms, _Options) ->
    set_mod_info(Forms),
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
set_mod_info([{attribute, _, file, {FileName, _}}|_]) ->
    erlang:put(mixer_delegate_file, FileName);
set_mod_info([{attribute, _, module, Mod}|_]) ->
    erlang:put(mixer_calling_mod, Mod).

get_file_name() ->
    erlang:get(mixer_delegate_file).

%% get_calling_mod() ->
%%     erlang:get(mixer_calling_mod).

finalize(Mixins, NewEOF, Forms) ->
    insert_exports(Mixins, Forms, []) ++ [{eof, NewEOF}].

insert_exports([], Forms, Accum) ->
    Accum ++ Forms;
insert_exports([#mixin{line=Line}|_]=Mixins, [{attribute, Line, mixin, _}|FT], Accum) ->
    {Exports, Mixins1} = make_export_statement(Line, Mixins),
    insert_exports(Mixins1, FT, Accum ++ Exports);
insert_exports([#mixin{line=Line}|_]=Mixins, [], Accum) ->
    {Exports, Mixins1} = make_export_statement(Line,  Mixins),
    insert_exports(Mixins1, [], Accum ++ Exports);
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

parse_and_expand_mixins([], []) ->
    [];
parse_and_expand_mixins([], Accum) ->
    group_mixins({none, 0}, lists:keysort(2, Accum), []);
parse_and_expand_mixins([{attribute, Line, mixin, Mixins0}|T], Accum) when is_list(Mixins0) ->
    Mixins = [expand_mixin(Line, Mixin) || Mixin <- Mixins0],
    parse_and_expand_mixins(T, lists:flatten([Accum, Mixins]));
parse_and_expand_mixins([_|T], Accum) ->
    parse_and_expand_mixins(T, Accum).

group_mixins(_, [], Accum) ->
    lists:keysort(2, Accum);
group_mixins({CMod, CLine}, [#mixin{mod=CMod, line=CLine}=H|T], Accum) ->
    group_mixins({CMod, CLine}, T, [H|Accum]);
group_mixins({CMod, CLine}, [#mixin{mod=CMod}=H|T], Accum) ->
    group_mixins({CMod, CLine}, T, [H#mixin{line=CLine}|Accum]);
group_mixins({_CMod, _}, [#mixin{mod=Mod, line=Line}=H|T], Accum) ->
    group_mixins({Mod, Line}, T, [H|Accum]).

expand_mixin(Line, Name) when is_atom(Name) ->
    case catch Name:module_info(exports) of
        {'EXIT', _} ->
            io:format("~s:~p Unable to resolve imported module ~p~n", [get_file_name(), Line, Name]),
            exit({error, undef_mixin_module});
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
find_dupe(Fun, Arity, [_|T]) ->
    find_dupe(Fun, Arity, T).

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
