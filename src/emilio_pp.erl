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
    Linearized = lists:flatmap(fun linearize/1, Forms),
    Rewhitespaced = rewhitespace(MacroedTokens),
    Reinserted = reinsert_tokens(Rewhitespaced, Linearized),
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


linearize({attribute, _Anno, module, _Name} = Elem) ->
    [Elem];

linearize({attribute, _Anno, behavior, _Name} = Elem) ->
    [Elem];

linearize({attribute, _Anno, beahviour, _Name} = Elem) ->
    [Elem];

linearize({attribute, Anno, export, FAList}) ->
    Exports = [{export, Anno, F, A} || {F, A} <- FAList],
    [{attribute, Anno, export} | Exports];

linearize({attribute, Anno, import, {Module, FAList}}) ->
    Imports = [{import, Anno, F, A} || {F, A} <- FAList],
    [{attribute, Anno, import, Module} | Imports];

linearize({attribute, Anno, export_type, TAList}) ->
    Types = [{export_type, Anno, T, A} || {T, A} <- TAList],
    [{attribute, Anno, export_type, length(TAList)} | Types];

linearize({attribute, _Anno, compile, _Value} = Elem) ->
    [Elem];

linearize({attribute, _Anno1, file, {_Name, _Anno2}} = Elem) ->
    [Elem];

linearize({attribute, Anno, define, Args}) ->
    LinearArgs = lists:flatmap(fun linearize_expr/1, Args),
    [{attribute, Anno, define, length(Args)}] ++ LinearArgs;

linearize({attribute, Anno, record, {Name, Fields}}) ->
    LinearFields = lists:flatmap(fun linearize/1, Fields),
    [{attribute, Anno, record, Name, length(Fields)} | LinearFields];

linearize({record_field, Anno, Name}) ->
    [{record_field, Anno, Name}];

linearize({record_field, Anno, Name, Initializer}) ->
    LinearInit = linearize(Initializer),
    [{record_field, Anno, Name}] ++ [{record_field_init, Anno}] ++ LinearInit;

linearize({typed_record_field, Field, Type}) ->
    LinearField = linearize(Field),
    LinearType = linearize_type(Type),
    LinearField ++ [{record_field_type, element(2, Field)}] ++ LinearType;

linearize({attribute, Anno, TypeAttr, {Name, Type, Vars}}) ->
    LinearVars = lists:flatmap(fun linearize_expr/1, Vars),
    [{attribute, Anno, TypeAttr, Name, length(Vars)}]
            ++ LinearVars
            ++ linearize_type(Type);

linearize({attribute, Anno, SpecAttr, {{Name, Arity}, TypeList}}) ->
    LinearTypes = lists:flatmap(fun linearize_type/1, TypeList),
    [{attribute, Anno, SpecAttr, {Name, Arity}, length(TypeList)}]
            ++ LinearTypes;

linearize({attribute, Anno, SpecAttr, {{Mod, Name, Arity}, TypeList}}) ->
    LinearTypes = lists:flatmap(fun linearize_type/1, TypeList),
    [{attribute, Anno, SpecAttr, {Mod, Name, Arity}, length(TypeList)}]
            ++ LinearTypes;

linearize({attribute, _Anno, _Name, _Value} = Elem) ->
    [Elem];

linearize({function, Anno, Name, Arity, Clauses}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(function_clause, C)
    end, Clauses),
    [{function, Anno, Name, Arity, length(Clauses)}] ++ LinearClauses.


linearize_type({ann_type, Anno, [AfAnno | SubTypes]}) ->
    [{ann_type, Anno, length(SubTypes)}]
            ++ linearize_type(AfAnno)
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({type, Anno, binary, IntParams}) ->
    [{type, Anno, binary, length(IntParams)}]
            ++ lists:flatmap(fun linearize_type/1, IntParams);

linearize_type({type, Anno, nil, []}) ->
    [{type, Anno, nil}];

linearize_type({type, Anno, 'fun', []}) ->
    [{type, Anno, 'fun'}];

linearize_type({type, Anno, 'fun', [{type, Anno, 'any'}, ReturnType]}) ->
    [{type, Anno, 'fun', return}]
            ++ linearize_type(ReturnType);

linearize_type({type, Anno, 'fun', FunctionType}) ->
    [{type, _, product, ArgTypes}, ReturnType] = FunctionType,
    [{type, Anno, 'fun', length(ArgTypes)}]
            ++ lists:flatmap(fun linearize_type/1, ArgTypes)
            ++ linearize_type(ReturnType);

linearize_type({type, Anno, bounded_fun, [FunType, Constraints]}) ->
    [{type, Anno, bounded_fun, length(Constraints)}]
            ++ linearize_type(FunType)
            ++ lists:flatmap(fun linearize_type/1, Constraints);

linearize_type({type, Anno, constraint, [{_, _, is_subtype}, [Var, Type]]}) ->
    [{type, Anno, constraint, is_subtype}]
            ++ linearize_type(Var)
            ++ linearize_type(Type);

linearize_type({type, Anno, range, IntTypes}) ->
    [{type, Anno, range, length(IntTypes)}]
            ++ lists:flatmap(fun linearize_type/1, IntTypes);

linearize_type({type, Anno, map, any}) ->
    [{type, Anno, map}];

linearize_type({type, Anno, map, AssocTypes}) ->
    [{type, Anno, map, length(AssocTypes)}]
            ++ lists:flatmap(fun linearize_type/1, AssocTypes);

linearize_type({type, Anno, map_field_assoc, SubTypes}) ->
    [{type, Anno, map_field_assoc, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({type, Anno, map_field_exact, SubTypes}) ->
    [{type, Anno, map_field_exact, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({type, Anno, 'record', [Name | FieldTypes]}) ->
    [{type, Anno, 'record', Name, length(FieldTypes)}]
            ++ linearize_type(Name)
            ++ lists:flatmap(fun linearize_type/1, FieldTypes);

linearize_type({type, Anno, field_type, [Name, Type]}) ->
    [{type, Anno, field_type}]
            ++ linearize_type(Name)
            ++ linearize_type(Type);

linearize_type({remote_type, Anno, [Mod, TypeName, ArgTypes]}) ->
    [{remote_type, Anno, length(ArgTypes)}]
            ++ linearize_type(Mod)
            ++ linearize_type(TypeName)
            ++ lists:flatmap(fun linearize_type/1, ArgTypes);

linearize_type({type, Anno, tuple, any}) ->
    [{type, Anno, tuple}];

linearize_type({type, Anno, tuple, SubTypes}) ->
    [{type, Anno, tupl, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({type, Anno, union, SubTypes}) ->
    [{type, Anno, union, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({user_type, Anno, TypeName, SubTypes}) ->
    [{user_type, Anno, TypeName, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type({type, Anno, TypeName, SubTypes}) ->
    [{type, Anno, TypeName, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes);

linearize_type(Else) ->
    linearize_expr(Else).


linearize_expr({atom, _Anno, _Atom} = Elem) ->
    [Elem];

linearize_expr({char, _Anno, _Char} = Elem) ->
    [Elem];

linearize_expr({float, _Anno, _Float} = Elem) ->
    [Elem];

linearize_expr({integer, _Anno, _Int} = Elem) ->
    [Elem];

linearize_expr({string, _Anno, _String} = Elem) ->
    [Elem];

linearize_expr({match, Anno, Pattern, Match}) ->
    Left = linearize_expr(Pattern),
    Right = linearize_expr(Match),
    Left ++ [{match, Anno}] ++ Right;

linearize_expr({var, _Anno, _Atom} = Elem) ->
    [Elem];

linearize_expr({macro, _Anno, _Atom} = Elem) ->
    [Elem];

linearize_expr({tuple, Anno, Elems}) ->
    LinearElems = lists:flatmap(fun linearize_expr/1, Elems),
    [{tuple, Anno, length(Elems)}] ++ LinearElems;

linearize_expr({nil, Anno}) ->
    [{nil, Anno}];

linearize_expr({cons, Anno, Head, Tail}) ->
    LinearHead = linearize_expr(Head),
    LinearTail = linearize_expr(Tail),
    [{cons, Anno}] ++ LinearHead ++ LinearTail;

linearize_expr({bin, Anno, Elems}) ->
    LinearElems = lists:flatmap(fun linearize_expr/1, Elems),
    [{bin, Anno, length(Elems)}] ++ LinearElems;

linearize_expr({bin_element, Anno, Expr, Size, TSL}) ->
    [{bin_element, Anno}]
            ++ linearize_expr(Expr)
            ++ [{bin_size, Anno, Size}, {bin_tsl, Anno, TSL}];

linearize_expr({op, Anno, Op, Left, Right}) ->
    LinearLeft = linearize_expr(Left),
    LinearRight = linearize_expr(Right),
    LinearLeft ++ [{op, Anno, Op}] ++ LinearRight;

linearize_expr({op, Anno, Op, Right}) ->
    LinearRight = linearize_expr(Right),
    [{op, Anno, Op}] ++ LinearRight;

linearize_expr({record, Anno, Name, Fields}) ->
    LinearFields = lists:flatmap(fun linearize_expr/1, Fields),
    [{record, Anno, Name, length(Fields)}] ++ LinearFields;

linearize_expr({record, Anno, Expr, Name, Fields}) ->
    LinearExpr = linearize_expr(Expr),
    LinearFields = lists:flatmap(fun linearize_expr/1, Fields),
    [{record_update, Anno, Name, length(Fields)}]
            ++ LinearExpr
            ++ LinearFields;

linearize_expr({record_index, _Anno, _Name, _Field} = Elem) ->
    [Elem];

linearize_expr({record_field, Anno, Name, Expr}) ->
    LinearExpr = linearize_expr(Expr),
    [{record_field, Anno, Name}] ++ LinearExpr;

linearize_expr({record_field, Anno, Expr, Name, Field}) ->
    LinearExpr = linearize_expr(Expr),
    [{record_field, Anno, Name}] ++ LinearExpr ++ [Field];

linearize_expr({map, Anno, Assocs}) ->
    LinearAssocs = lists:flatmap(fun linearize_expr/1, Assocs),
    [{map, Anno, length(Assocs)}] ++ LinearAssocs;

linearize_expr({map, Anno, Expr, Assocs}) ->
    LinearExpr = linearize_expr(Expr),
    LinearAssocs = lists:map(fun linearize_expr/1, Assocs),
    [{map_update, Anno, length(Assocs)}] ++ LinearExpr ++ LinearAssocs;

linearize_expr({map_field_assoc, Anno, Key, Val}) ->
    LinearKey = linearize_expr(Key),
    LinearVal = linearize_expr(Val),
    [{map_field_assoc, Anno}] ++ LinearKey ++ LinearVal;

linearize_expr({map_field_exact, Anno, Key, Val}) ->
    LinearKey = linearize_expr(Key),
    LinearVal = linearize_expr(Val),
    [{map_field_exact, Anno}] ++ LinearKey ++ LinearVal;

linearize_expr({'catch', Anno, Expr}) ->
    LinearExpr = linearize_expr(Expr),
    [{'catch', Anno}] ++ LinearExpr;

linearize_expr({call, Anno, {remote, Anno, Mod, Fun}, Args}) ->
    LinearMod = linearize_expr(Mod),
    LinearFun = linearize_expr(Fun),
    LinearArgs = lists:flatmap(fun linearize_expr/1, Args),
    [{call_remote, Anno, length(Args)}]
            ++ LinearMod
            ++ LinearFun
            ++ LinearArgs;

linearize_expr({call, Anno, Fun, Args}) ->
    LinearFun = linearize_expr(Fun),
    LinearArgs = lists:flatmap(fun linearize_expr/1, Args),
    [{call, Anno, length(Args)}] ++ LinearFun ++ LinearArgs;

linearize_expr({lc, Anno, Template, Qualifiers}) ->
    LinearTemplate = linearize_expr(Template),
    LinearQualifiers = lists:flatmap(fun linearize_expr/1, Qualifiers),
    [{lc, Anno, length(Qualifiers)}] ++ LinearTemplate ++ LinearQualifiers;

linearize_expr({bc, Anno, Template, Qualifiers}) ->
    LinearTemplate = linearize_expr(Template),
    LinearQualifiers = lists:flatmap(fun linearize_expr/1, Qualifiers),
    [{bc, Anno, length(Qualifiers)}] ++ LinearTemplate ++ LinearQualifiers;

linearize_expr({generate, Anno, Pattern, Expr}) ->
    LinearPattern = linearize_expr(Pattern),
    LinearExpr = linearize_expr(Expr),
    [{generate, Anno}] ++ LinearPattern ++ LinearExpr;

linearize_expr({b_generate, Anno, Pattern, Expr}) ->
    LinearPattern = linearize_expr(Pattern),
    LinearExpr = linearize_expr(Expr),
    [{b_generate, Anno}] ++ LinearPattern ++ LinearExpr;

linearize_expr({block, Anno, Body}) ->
    LinearBody = lists:flatmap(fun linearize_expr/1, Body),
    [{block_start, Anno}] ++ LinearBody;

linearize_expr({'if', Anno, Clauses}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(if_clause, C)
    end, Clauses),
    [{'if', Anno, length(Clauses)}] ++ LinearClauses;

linearize_expr({'case', Anno, Expr, Clauses}) ->
    LinearExpr = linearize_expr(Expr),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(case_clause, C)
    end, Clauses),
    [{'case', Anno, length(Clauses)}] ++ LinearExpr ++ LinearClauses;

linearize_expr({'try', Anno, Body, Cases, Catches, After}) ->
    LinearBody = if Body == [] -> []; true ->
        lists:flatmap(fun linearize_expr/1, Body)
    end,
    LinearCases = if Cases == [] -> []; true ->
        lists:flatmap(fun(C) ->
            linearize_clause(try_case_clause, C)
        end, Cases)
    end,
    LinearCatches = if Catches == [] -> []; true ->
        lists:flatmap(fun(C) ->
            linearize_clause(try_catch_clause, C)
        end, Catches)
    end,
    LinearAfter = if After == [] -> []; true ->
        lists:flatmap(fun linearize_expr/1, After)
    end,
    [{
        'try',
        Anno,
        length(Body),
        length(Cases),
        length(Catches),
        length(After)
    }]
            ++ LinearBody
            ++ LinearCases
            ++ LinearCatches
            ++ LinearAfter;

linearize_expr({'receive', Anno, Clauses}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(receive_clause, C)
    end, Clauses),
    [{'receive', Anno, length(Clauses)}] ++ LinearClauses;

linearize_expr({'receive', Anno, Clauses, Timeout, After}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(receive_clause, C)
    end, Clauses),
    LinearTimeout = linearize_expr(Timeout),
    LinearAfter = lists:flatmap(fun linearize_expr/1, After),
    [{'receive', Anno, length(Clauses)}]
            ++ LinearClauses
            ++ [{'receive_timeout', Anno}]
            ++ LinearTimeout
            ++ [{'receive_after', Anno, length(After)}]
            ++ LinearAfter;

linearize_expr({'fun', _Anno, {function, _Name, _Arity}} = Elem) ->
    [Elem];

linearize_expr({'fun', _Ann0, {function, _Mod, _Name, _Arity}} = Elem) ->
    [Elem];

linearize_expr({'fun', Anno, {clauses, Clauses}}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(fun_clause, C)
    end, Clauses),
    [{'fun', Anno, length(Clauses)}] ++ LinearClauses;

linearize_expr({named_fun, Anno, Name, Clauses}) ->
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(named_fun_clause, C)
    end, Clauses),
    [{named_fun, Anno, Name, length(Clauses)}] ++ LinearClauses.


linearize_clause(Type, {clause, Anno, Patterns, Guards, Body}) ->
    LinearPatterns = lists:flatmap(fun linearize_expr/1, Patterns),
    LinearGuards = lists:flatmap(fun linearize_guards/1, Guards),
    LinearBody = lists:flatmap(fun linearize_expr/1, Body),
    [{Type, Anno, length(Patterns), length(Guards)}]
            ++ LinearPatterns
            ++ LinearGuards
            ++ LinearBody.


linearize_guards(Guards) ->
    LinearGuards = lists:flatmap(fun linearize_expr/1, Guards),
    [{guard, element(2, hd(Guards)), length(Guards)}] ++ LinearGuards.


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
