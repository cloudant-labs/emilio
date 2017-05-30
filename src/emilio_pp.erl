% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(emilio_pp).


-export([
    file/1
]).


-define(SCAN_OPTS, [text, return]).


file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    {ok, AllTokens, _} = erl_scan:string(
            binary_to_list(Data),
            {1, 1},
            ?SCAN_OPTS
        ),
    MacroedTokens = macroize(AllTokens),
    {CodeTokens, _NonCodeTokens} = split_code(MacroedTokens),
    Forms = parse_forms(CodeTokens, []),
    Linearized = lists:flatmap(fun(F) -> linearize(F, 0) end, Forms),
    Rewhitespaced = rewhitespace(MacroedTokens),
    Reinserted = reinsert_tokens(Rewhitespaced, Linearized),
    io:format(standard_error, "~5000p~n", [Reinserted]),
    DeTexted = detextify(Reinserted),
    group_lines(DeTexted).


split_code([]) ->
    {[], []};

split_code([Token | Rest]) when element(1, Token) == comment ->
    {Code, NonCode} = split_code(Rest),
    {Code, [Token | NonCode]};

split_code([Token | Rest]) when element(1, Token) == white_space ->
    {Code, NonCode} = split_code(Rest),
    {Code, [Token | NonCode]};

split_code([Token | Rest]) ->
    {Code, NonCode} = split_code(Rest),
    {[Token | Code], NonCode}.


macroize([]) ->
    [];

macroize([{'?', Anno1}, {atom, _, Name} | Rest]) ->
    NewText = [$? | atom_to_list(Name)],
    NewToken = {macro, Anno1, list_to_atom(NewText)},
    [NewToken | macroize(Rest)];

macroize([{'?', _} = T1, {var, Anno, Name} | Rest]) ->
    macroize([T1, {atom, Anno, Name} | Rest]);

macroize([Token | Rest]) ->
    [Token | macroize(Rest)].


parse_forms([], Acc) ->
    lists:reverse(Acc);

parse_forms([{eof, _}], Acc) ->
    lists:reverse(Acc);

parse_forms(Tokens, Acc) ->
    IsNotDot = fun(Token) -> element(1, Token) /= dot end,
    {Form, [Dot | Rest]} = lists:splitwith(IsNotDot, Tokens),
    FormTokens = macroize(Form ++ [Dot]),
    Result = case emilio_erl_parse:parse_form(FormTokens) of
        {ok, AbstractForm} ->
            AbstractForm;
        {error, Descriptor} ->
            {error, emilio_erl_parse:format_error(Descriptor)}
    end,
    parse_forms(Rest, [Result | Acc]).



linearize({attribute, _Anno, module, _Name} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({attribute, _Anno, behavior, _Name} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({attribute, _Anno, beahviour, _Name} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({attribute, Anno, export, FAList}, Depth) ->
    Exports = lists:map(fun({F, A}) ->
        hd(set_depth({export, Anno, F, A}, Depth + 1))
    end, FAList),
    set_depth({attribute, Anno, export}, Depth) ++ Exports;

linearize({attribute, Anno, import, {Module, FAList}}, Depth) ->
    Imports = lists:map(fun({F, A}) ->
        hd(set_depth({import, Anno, F, A}, Depth + 1))
    end, FAList),
    set_depth({attribute, Anno, import, Module}, Depth) ++ Imports;

linearize({attribute, Anno, export_type, TAList}, Depth) ->
    ExportTypes = lists:map(fun({T, A}) ->
        hd(set_depth({export_type, Anno, T, A}, Depth + 1))
    end, TAList),
    set_depth({attribute, Anno, export_type, length(TAList)}, Depth)
            ++ ExportTypes;

linearize({attribute, _Anno, compile, _Value} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({attribute, _Anno1, file, {_Name, _Anno2}} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({attribute, Anno, define, Args}, Depth) ->
    set_depth({attribute, Anno, define, length(Args)}, Depth)
            ++ linearize_expr_list(Args, Depth + 1);

linearize({attribute, Anno, record, {Name, Fields}}, Depth) ->
    set_depth({attribute, Anno, record, Name, length(Fields)}, Depth)
            ++ linearize_list(Fields, Depth + 1);

linearize({record_field, Anno, Name}, Depth) ->
    set_depth({record_field, Anno, Name}, Depth);

linearize({record_field, Anno, Name, Initializer}, Depth) ->
    set_depth({record_field, Anno, Name}, Depth)
            ++ set_depth({record_field_init, Anno}, Depth + 1)
            ++ linearize(Initializer, Depth + 2);

linearize({typed_record_field, Field, Type}, Depth) ->
    linearize(Field, Depth)
            ++ set_depth({record_field_type, element(2, Field)}, Depth)
            ++ linearize_type(Type, Depth);

linearize({attribute, Anno, TypeAttr, {Name, Type, Vars}}, Depth) ->
    set_depth({attribute, Anno, TypeAttr, Name, length(Vars)}, Depth)
            ++ linearize_expr_list(Vars, Depth)
            ++ linearize_type(Type, Depth);

linearize({attribute, Anno, SpecAttr, {{Name, Arity}, TypeList}}, Depth) ->
    Elem = {attribute, Anno, SpecAttr, {Name, Arity}, length(TypeList)},
    set_depth(Elem, Depth) ++ linearize_type_list(TypeList, Depth);

linearize({attribute, Anno, SpecAttr, {{Mod, Name, Arity}, TypeList}}, Depth) ->
    Elem = {attribute, Anno, SpecAttr, {Mod, Name, Arity}, length(TypeList)},
    set_depth(Elem, Depth) ++ linearize_type_list(TypeList, Depth);

linearize({attribute, _Anno, _Name, _Value} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize({function, Anno, Name, Arity, Clauses}, Depth) ->
    set_depth({function, Anno, Name, Arity, length(Clauses)}, Depth)
            ++ linearize_clause_list(function_clause, Clauses, Depth).


linearize_type({ann_type, Anno, [AfAnno | SubTypes]}, Depth) ->
    set_depth({ann_type, Anno, length(SubTypes)}, Depth)
            ++ linearize_type(AfAnno, Depth)
            ++ linearize_type_list(SubTypes, Depth);

linearize_type({type, Anno, binary, IntParams}, Depth) ->
    set_depth({type, Anno, binary, length(IntParams)}, Depth)
            ++ linearize_type_list(IntParams, Depth);

linearize_type({type, Anno, nil, []}, Depth) ->
    set_depth({type, Anno, nil}, Depth);

linearize_type({type, Anno, 'fun', []}, Depth) ->
    set_depth({type, Anno, 'fun'}, Depth);

linearize_type({type, Anno, 'fun', [{type, Anno, 'any'}, ReturnType]}, Depth) ->
    set_depth({type, Anno, 'fun'}, Depth)
            ++ linearize_type(ReturnType, Depth);

linearize_type({type, Anno, 'fun', FunctionType}, Depth) ->
    [{type, _, product, ArgTypes}, ReturnType] = FunctionType,
    set_depth({type, Anno, 'fun', length(ArgTypes)}, Depth)
            ++ linearize_type_list(ArgTypes, Depth)
            ++ linearize_type(ReturnType, Depth);

linearize_type({type, Anno, bounded_fun, [FunType, Constraints]}, Depth) ->
    set_depth({type, Anno, bounded_fun, length(Constraints)}, Depth)
            ++ linearize_type(FunType, Depth)
            ++ linearize_type_list(Constraints, Depth);

linearize_type({type, Anno, constraint, [{_, _, _}, [Var, Type]]}, Depth) ->
    set_depth({type, Anno, constraint, is_subtype}, Depth)
            ++ linearize_type(Var, Depth)
            ++ linearize_type(Type, Depth);

linearize_type({type, Anno, range, IntTypes}, Depth) ->
    set_depth({type, Anno, range, length(IntTypes)}, Depth)
            ++ linearize_type_list(IntTypes, Depth);

linearize_type({type, Anno, map, any}, Depth) ->
    set_depth({type, Anno, map}, Depth);

linearize_type({type, Anno, map, AssocTypes}, Depth) ->
    set_depth({type, Anno, map, length(AssocTypes)}, Depth)
            ++ linearize_type_list(AssocTypes, Depth);

linearize_type({type, Anno, map_field_assoc, SubTypes}, Depth) ->
    set_depth({type, Anno, map_field_assoc, length(SubTypes)}, Depth)
            ++ linearize_type_list(SubTypes, Depth);

linearize_type({type, Anno, map_field_exact, SubTypes}, Depth) ->
    set_depth({type, Anno, map_field_exact, length(SubTypes)}, Depth)
            ++ linearize_type_list(SubTypes, Depth);

linearize_type({type, Anno, 'record', [Name | FieldTypes]}, Depth) ->
    set_depth({type, Anno, 'record', Name, length(FieldTypes)}, Depth)
            ++ linearize_type(Name, Depth)
            ++ linearize_type_list(FieldTypes, Depth + 1);

linearize_type({type, Anno, field_type, [Name, Type]}, Depth) ->
    set_depth({type, Anno, field_type}, Depth)
            ++ linearize_type(Name, Depth)
            ++ linearize_type(Type, Depth);

linearize_type({remote_type, Anno, [Mod, TypeName, ArgTypes]}, Depth) ->
    set_depth({remote_type, Anno, length(ArgTypes)}, Depth)
            ++ linearize_type(Mod, Depth)
            ++ linearize_type(TypeName, Depth)
            ++ linearize_type_list(ArgTypes, Depth);

linearize_type({type, Anno, tuple, any}, Depth) ->
    set_depth({type, Anno, tuple}, Depth);

linearize_type({type, Anno, tuple, SubTypes}, Depth) ->
    set_depth({type, Anno, tupl, length(SubTypes)}, Depth)
            ++ linearize_type_list(SubTypes, Depth + 1);

linearize_type({type, Anno, union, SubTypes}, Depth) ->
    set_depth({type, Anno, union, length(SubTypes)}, Depth)
            ++ linearize_type_list(SubTypes, Depth + 1);

linearize_type({user_type, Anno, TypeName, SubTypes}, Depth) ->
    [{user_type, Anno, TypeName, length(SubTypes)}]
            ++ linearize_type_list(SubTypes, Depth);

linearize_type({type, Anno, TypeName, SubTypes}, Depth) ->
    [{type, Anno, TypeName, length(SubTypes)}]
            ++ linearize_type_list(SubTypes, Depth);

linearize_type(Else, Depth) ->
    linearize_expr(Else, Depth).


linearize_expr({atom, _Anno, _Atom} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({char, _Anno, _Char} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({float, _Anno, _Float} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({integer, _Anno, _Int} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({string, _Anno, _String} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({match, Anno, Pattern, Match}, Depth) ->
    linearize_expr(Pattern, Depth)
            ++ set_depth({match, Anno}, Depth)
            ++ linearize_expr(Match, Depth);

linearize_expr({var, _Anno, _Atom} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({macro, _Anno, _Atom} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({tuple, Anno, Elems}, Depth) ->
    set_depth({tuple, Anno, length(Elems)}, Depth)
            ++ linearize_expr_list(Elems, Depth + 1);

linearize_expr({nil, Anno}, Depth) ->
    set_depth({nil, Anno}, Depth);

linearize_expr({cons, Anno, Head, Tail}, Depth) ->
    set_depth({cons, Anno}, Depth)
            ++ linearize_expr(Head, Depth + 1)
            ++ linearize_expr(Tail, Depth);

linearize_expr({bin, Anno, Elems}, Depth) ->
    set_depth({bin, Anno, length(Elems)}, Depth)
            ++ linearize_expr_list(Elems, Depth + 1);

linearize_expr({bin_element, Anno, Expr, Size, TSL}, Depth) ->
    set_depth({bin_element, Anno}, Depth)
            ++ linearize_expr(Expr, Depth)
            ++ set_depth({bin_size, Anno, Size}, Depth)
            ++ set_depth({bin_tsl, Anno, TSL}, Depth);

linearize_expr({op, Anno, Op, Left, Right}, Depth) ->
    linearize_expr(Left, Depth)
            ++ set_depth({op, Anno, Op}, Depth)
            ++ linearize_expr(Right, Depth);

linearize_expr({op, Anno, Op, Right}, Depth) ->
    set_depth({op, Anno, Op}, Depth)
            ++ linearize_expr(Right, Depth);

linearize_expr({record, Anno, Name, Fields}, Depth) ->
    set_depth({record, Anno, Name, length(Fields)}, Depth)
            ++ linearize_expr_list(Fields, Depth + 1);

linearize_expr({record, Anno, Expr, Name, Fields}, Depth) ->
    set_depth({record_update, Anno, Name, length(Fields)}, Depth)
            ++ linearize_expr(Expr, Depth)
            ++ linearize_expr_list(Fields, Depth);

linearize_expr({record_index, _Anno, _Name, _Field} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({record_field, Anno, Name, Expr}, Depth) ->
    set_depth({record_field, Anno, Name}, Depth)
            ++ linearize_expr(Expr, Depth);

linearize_expr({record_field, Anno, Expr, Name, Field}, Depth) ->
    set_depth({record_field, Anno, Name}, Depth)
            ++ linearize_expr(Expr, Depth)
            ++ linearize_expr(Field, Depth);

linearize_expr({map, Anno, Assocs}, Depth) ->
    set_depth({map, Anno, length(Assocs)}, Depth)
            ++ linearize_expr_list(Assocs, Depth + 1);

linearize_expr({map, Anno, Expr, Assocs}, Depth) ->
    set_depth({map_update, Anno, length(Assocs)}, Depth)
            ++ linearize_expr(Expr, Depth)
            ++ linearize_expr_list(Assocs, Depth + 1);

linearize_expr({map_field_assoc, Anno, Key, Val}, Depth) ->
    set_depth({map_field_assoc, Anno, Key, Val}, Depth)
            ++ linearize_expr(Key, Depth)
            ++ linearize_expr(Val, Depth);

linearize_expr({map_field_exact, Anno, Key, Val}, Depth) ->
    set_depth({map_field_exact, Anno}, Depth)
            ++ linearize_expr(Key, Depth)
            ++ linearize_expr(Val, Depth);

linearize_expr({'catch', Anno, Expr}, Depth) ->
    set_depth({'catch', Anno}, Depth)
            ++ linearize_expr(Expr, Depth);

linearize_expr({call, Anno, {remote, Anno, Mod, Fun}, Args}, Depth) ->
    set_depth({call_remote, Anno, length(Args)}, Depth)
            ++ linearize_expr(Mod, Depth)
            ++ linearize_expr(Fun, Depth)
            ++ linearize_expr_list(Args, Depth);

linearize_expr({call, Anno, Fun, Args}, Depth) ->
    set_depth({call, Anno, length(Args)}, Depth)
            ++ linearize_expr(Fun, Depth)
            ++ linearize_expr_list(Args, Depth);

linearize_expr({lc, Anno, Template, Qualifiers}, Depth) ->
    set_depth({lc, Anno, length(Qualifiers)}, Depth)
            ++ linearize_expr(Template, Depth + 1)
            ++ linearize_expr_list(Qualifiers, Depth + 1);

linearize_expr({bc, Anno, Template, Qualifiers}, Depth) ->
    set_depth({bc, Anno, length(Qualifiers)}, Depth)
            ++ linearize_expr(Template, Depth + 1)
            ++ linearize_expr_list(Qualifiers, Depth + 1);

linearize_expr({generate, Anno, Pattern, Expr}, Depth) ->
    set_depth({generate, Anno}, Depth)
            ++ linearize_expr(Pattern, Depth)
            ++ linearize_expr(Expr, Depth);

linearize_expr({b_generate, Anno, Pattern, Expr}, Depth) ->
    set_depth({b_generate, Anno}, Depth)
            ++ linearize_expr(Pattern, Depth)
            ++ linearize_expr(Expr, Depth);

linearize_expr({block, Anno, Body}, Depth) ->
    set_depth({block_start, Anno}, Depth)
            ++ linearize_expr_list(Body, Depth + 1);

linearize_expr({'if', Anno, Clauses}, Depth) ->
    set_depth({'if', Anno, length(Clauses)}, Depth)
            ++ linearize_clause_list(if_clause, Clauses, Depth + 1);

linearize_expr({'case', Anno, Expr, Clauses}, Depth) ->
    set_depth({'case', Anno, length(Clauses)}, Depth)
            ++ linearize_expr(Expr, Depth)
            ++ linearize_clause_list(case_clause, Clauses, Depth + 1);

linearize_expr({'try', Anno, Body, Cases, Catches, After}, Depth) ->
    LinearBody = if Body == [] -> []; true ->
        linearize_expr_list(Body, Depth)
    end,
    LinearCases = if Cases == [] -> []; true ->
        linearize_clause_list(try_case_clause, Cases, Depth + 1)
    end,
    LinearCatches = if Catches == [] -> []; true ->
        linearize_clause_list(try_catch_clause, Cases, Depth + 1)
    end,
    LinearAfter = if After == [] -> []; true ->
        linearize_expr_list(After, Depth + 1)
    end,
    [set_depth({
        'try',
        Anno,
        length(Body),
        length(Cases),
        length(Catches),
        length(After)
    }, Depth)]
            ++ LinearBody
            ++ LinearCases
            ++ LinearCatches
            ++ LinearAfter;

linearize_expr({'receive', Anno, Clauses}, Depth) ->
    set_depth({'receive', Anno, length(Clauses)}, Depth)
            ++ linearize_clause_list(receive_clause, Clauses, Depth + 1);

linearize_expr({'receive', Anno, Clauses, Timeout, After}, Depth) ->
    set_depth({'receive', Anno, length(Clauses)}, Depth)
            ++ linearize_clause_list(receive_clause, Clauses, Depth + 1)
            ++ set_depth({'receive_timeout', Anno}, Depth)
            ++ linearize_expr(Timeout, Depth + 1)
            ++ set_depth({'receive_after', Anno}, Depth)
            ++ linearize_expr_list(After, Depth + 1);

linearize_expr({'fun', _Anno, {function, _Name, _Arity}} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({'fun', _Ann0, {function, _Mod, _Name, _Arity}} = Elem, Depth) ->
    set_depth(Elem, Depth);

linearize_expr({'fun', Anno, {clauses, Clauses}}, Depth) ->
    set_depth({'fun', Anno, length(Clauses)}, Depth)
            ++ linearize_clause_list(fun_clause, Clauses, Depth + 1);

linearize_expr({named_fun, Anno, Name, Clauses}, Depth) ->
    set_depth({named_fun, Anno, Name, length(Clauses)}, Depth)
            ++ linearize_clause_list(named_fun_clause, Clauses, Depth + 1).


linearize_clause(Type, {clause, Anno, Patterns, Guards, Body}, Depth) ->
    set_depth({Type, Anno, length(Patterns), length(Guards)}, Depth)
            ++ linearize_expr_list(Patterns, Depth)
            ++ linearize_guards(Guards, Depth)
            ++ linearize_expr_list(Body, Depth + 1).


linearize_guard_seq(Guards, Depth) ->
    set_depth({guard, element(2, hd(Guards))}, Depth)
            ++ linearize_expr_list(Guards, Depth).


linearize_list(Elems, Depth) ->
    lists:flatmap(fun(E) -> linearize(E, Depth) end, Elems).


linearize_type_list(Types, Depth) ->
    lists:flatmap(fun(T) -> linearize_type(T, Depth) end, Types).


linearize_expr_list(Exprs, Depth) ->
    lists:flatmap(fun(E) -> linearize_expr(E, Depth) end, Exprs).


linearize_clause_list(Type, Clauses, Depth) ->
    lists:flatmap(fun(C) -> linearize_clause(Type, C, Depth) end, Clauses).


linearize_guards(Guards, Depth) ->
    lists:flatmap(fun(G) -> linearize_guard_seq(G, Depth) end, Guards).


set_depth(Elem, Depth) ->
    Anno = element(2, Elem),
    NewAnno = lists:keystore(depth, 1, Anno, {depth, Depth}),
    [setelement(2, Elem, NewAnno)].


rewhitespace([]) ->
    [];

rewhitespace([{white_space, _Anno, Text} = Tok | RestTokens]) ->
    Loc = get_location(Tok),
    break_ws_token(Loc, Text) ++ rewhitespace(RestTokens);

rewhitespace([{dot, Anno} = Tok | RestTokens]) ->
    Loc = get_location(Tok),
    {text, Text} = lists:keyfind(text, 1, Anno),
    break_dot_token(Loc, Text) ++ rewhitespace(RestTokens);

rewhitespace([Token | RestTokens]) ->
    [Token] ++ rewhitespace(RestTokens).


break_ws_token(_Loc, []) ->
    [];

break_ws_token({Line, Col}, Text) ->
    Pred = fun(C) -> C /= $\n end,
    {Prefix, Rest} = lists:splitwith(Pred, Text),
    case length(Rest) > 1 of
        true ->
            Anno = [{line, Line}, {column, Col}],
            Tok = {white_space, Anno, Prefix ++ [$\n]},
            [Tok] ++ break_ws_token({Line + 1, 1}, tl(Rest));
        false ->
            [{white_space, [{line, Line}, {column, Col}], Text}]
    end.


break_dot_token({Line, Col}, [$.]) ->
    [{dot, [{line, Line}, {column, Col}]}];

break_dot_token({Line, Col}, [$., C]) ->
    DotTok = {dot, [{line, Line}, {column, Col}]},
    WsTok = {white_space, [{line, Line}, {column, Col + 1}], [C]},
    [DotTok, WsTok].


reinsert_tokens([], Nodes) ->
    Nodes;

reinsert_tokens(Tokens, []) ->
    Tokens;

reinsert_tokens([Token | RestTokens] = Tokens, [Node | RestNodes] = Nodes) ->
    TokLoc = get_location(Token),
    NodeLoc = get_location(Node),
    case {TokLoc, NodeLoc} of
        _ when TokLoc < NodeLoc ->
            Rest = reinsert_tokens(RestTokens, Nodes),
            [Token | Rest];
        _ when TokLoc > NodeLoc ->
            Rest = reinsert_tokens(Tokens, RestNodes),
            [Node | Rest];
        _ when TokLoc == NodeLoc ->
            reinsert_tokens(RestTokens, Nodes)
    end.


detextify([]) ->
    [];

detextify([Token | RestTokens]) ->
    Loc = get_location(Token),
    NewTok = setelement(2, Token, Loc),
    [NewTok] ++ detextify(RestTokens).


group_lines([]) ->
    [];

group_lines([Tok | Rest]) ->
    group_lines(Rest, [Tok], []).


group_lines([], [], GroupAcc) ->
    lists:reverse(GroupAcc);

group_lines([], Group, GroupAcc) ->
    lists:reverse(GroupAcc, [lists:reverse(Group)]);

group_lines([Token | RestTokens], [G | _] = Group, GroupAcc) ->
    {TLine, _} = element(2, Token),
    {GLine, _} = element(2, G),
    case TLine > GLine of
        true ->
            NewGroupAcc = [lists:reverse(Group) | GroupAcc],
            group_lines(RestTokens, [Token], NewGroupAcc);
        false ->
            group_lines(RestTokens, [Token | Group], GroupAcc)
    end.


get_location(Term) when is_tuple(Term), size(Term) >= 2 ->
    Anno = element(2, Term),
    {line, Line} = lists:keyfind(line, 1, Anno),
    {column, Column} = lists:keyfind(column, 1, Anno),
    {Line, Column}.
