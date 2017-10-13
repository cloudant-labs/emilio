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
    file/1,
    codes/0,
    format_error/2
]).


-include("emilio.hrl").


-define(SCAN_OPTS, [text, return]).


file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    DataList = binary_to_list(Data),
    case emilio_erl_scan:string(DataList, {1, 1}, ?SCAN_OPTS) of
        {ok, AllTokens, _} ->
            Reverted = revert_annos(AllTokens),
            MacroedTokens = macroize(Reverted),
            ReDefinedTokens = process_define_args(MacroedTokens),
            {CodeTokens, _NonCodeTokens} = split_code(ReDefinedTokens),
            Forms = parse_forms(CodeTokens, []),
            Linearized = lists:flatmap(fun linearize/1, Forms),
            Rewhitespaced = rewhitespace(ReDefinedTokens),
            Coalesced = coalesce_whitespace(Rewhitespaced),
            Reinserted = reinsert_tokens(Coalesced, Linearized),
            DeTexted = detextify(Reinserted),
            emilio_lib:group_lines(DeTexted);
        {error, {Loc, Module, Error}, _} ->
            Anno = case Loc of
                {Line, Col} ->
                    [{line, Line}, {column, Col}];
                _ ->
                    [{line, 0}, {column, 0}]
            end,
            Message = Module:format_error(Error),
            ?EMILIO_REPORT(Anno, 901, Message),
            []
    end.


codes() ->
    [901, 902].


format_error(901, Arg) ->
    io_lib:format("Unable to scan source: ~s", [Arg]);
format_error(902, Arg) ->
    io_lib:format("Unable to parse form: ~s", [Arg]).


split_code([]) ->
    {[], []};

split_code([Token | Rest]) ->
    {Code, NonCode} = split_code(Rest),
    case is_code(Token) of
        true ->
            {[Token | Code], NonCode};
        false ->
            {Code, [Token | NonCode]}
    end.


revert_annos([]) ->
    [];

revert_annos([Token | Rest]) ->
    Anno = element(2, Token),
    {value, {location, {Line, Col}}, RestAnno} =
            lists:keytake(location, 1, Anno),
    Ref = erlang:make_ref(),
    NewAnno = RestAnno ++ [{line, Line}, {column, Col}, {ref, Ref}],
    [setelement(2, Token, NewAnno)] ++ revert_annos(Rest).


macroize([]) ->
    [];

macroize([{'?', Anno} = MacroToken | Rest]) ->
    case extend_macro(Rest) of
        {MacroTokens, RestTokens} ->
            TextList = lists:foldl(fun(Token, Acc) ->
                case Token of
                    {'?', _} -> ["?" | Acc];
                    {atom, _, Name} -> [atom_to_list(Name) | Acc];
                    {var, _, Name} -> [atom_to_list(Name) | Acc]
                end
            end, ["?"], MacroTokens),
            Text = lists:flatten(lists:reverse(TextList)),
            NewToken = {macro, Anno, list_to_atom(Text)},
            RestMacroized = macroize(RestTokens),
            [NewToken] ++ replace_macro_args(RestMacroized);
        not_a_macro ->
            [MacroToken] ++ macroize(Rest)
    end;

macroize([Token | Rest]) ->
    [Token] ++ macroize(Rest).


extend_macro([{'?', _} = MacroToken | Rest]) ->
    case extend_macro(Rest) of
        {RestMacro, RestTokens} ->
            {[MacroToken | RestMacro], RestTokens};
        not_a_macro ->
            not_a_macro
    end;

extend_macro([{white_space, _, _} | Rest]) ->
    extend_macro(Rest);

extend_macro([{atom, _, _} = MacroToken | Rest]) ->
    {[MacroToken], Rest};

extend_macro([{var, _, _} = MacroToken | Rest]) ->
    {[MacroToken], Rest};

extend_macro(_) ->
    not_a_macro.


% The logic for replace_macro_args is based on skip_macro_args
% in epp_dodger.erl
%
% The logic here is that for every argument in a call
% to a macro we try to parse the expression. If it parses
% then we leave it alone. If it doesn't parse then we
% replace the argument with a stringified version.

replace_macro_args([{'(', _} = Token | Rest]) ->
    [Token] ++ replace_macro_args(Rest, [')'], []);

replace_macro_args([{white_space, _, _} = Token | Rest]) ->
    [Token] ++ replace_macro_args(Rest);

replace_macro_args(Tokens) ->
    Tokens.


replace_macro_args([{'(', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, [')' | CloseStack], [Token | Arg]);

replace_macro_args([{'{', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['}' | CloseStack], [Token | Arg]);

replace_macro_args([{'[', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, [']' | CloseStack], [Token | Arg]);

replace_macro_args([{'<<', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['>>' | CloseStack], [Token | Arg]);

replace_macro_args([{'begin', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{'if', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{'case', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{'receive', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{'try', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{'cond', _} = Token | Rest], CloseStack, Arg) ->
    replace_macro_args(Rest, ['end' | CloseStack], [Token | Arg]);

replace_macro_args([{',', Loc} = Token | Rest], [Close], Arg) ->
    % Note that the logic here is that we hit a comma
    % at the top level between '(' and ')' which indicates
    % that we've finished accumulating an argument.
    macro_argify(Arg, Loc) ++ [Token] ++ replace_macro_args(Rest, [Close], []);

replace_macro_args([{Close, Loc} = Token | Rest], [Close], Arg) ->
    % Need to check the last argument in case this was
    % a call with no arguments
    Replacement = case length(lists:filter(fun is_code/1, Arg)) of
        0 ->
            lists:reverse(Arg);
        _ ->
            macro_argify(Arg, Loc)
    end,
    Replacement ++ [Token] ++ Rest;

replace_macro_args([{Close, _} = Token | Rest], [Close | CloseStack], Arg) ->
    % Matched a closing element
    replace_macro_args(Rest, CloseStack, [Token | Arg]);

replace_macro_args([Token | Rest], CloseStack, Arg) ->
    % Anything else just goes into the current argument
    replace_macro_args(Rest, CloseStack, [Token | Arg]);

replace_macro_args([], _CloseStack, [{dot, _} = Dot | RestArg]) ->
    % We'll end up here if we had a macro definition that had
    % unlanced openings for complex expressions. If it looks
    % like we're in a define then we'll just go ahead and
    % stringify the arg after removing the trailing right
    % paren.
    {Arg, Close} = unwind_close_paren(RestArg),
    stringify_tokens(lists:reverse(Arg)) ++ Close ++ [Dot];

replace_macro_args([], _CloseStack, _Arg) ->
    erlang:error({error, macro_args}).


macro_argify(Arg, Loc) ->
    Code = lists:filter(fun is_code/1, Arg),
    Expr = lists:reverse(Code, [{dot, Loc}]),
    try
        {ok, ExprForms} = emilio_erl_parse:parse_exprs(Expr),
        lists:foreach(fun(E) -> linearize_expr(E, 0) end, ExprForms),
        lists:reverse(Arg)
    catch _:_ ->
        stringify_tokens(lists:reverse(Arg))
    end.


process_define_args([]) ->
    [];

process_define_args([{'-', _} = Token | Rest]) ->
    [Token] ++ process_define_args(find_define(Rest));

process_define_args([Token | Rest]) ->
    [Token] ++ process_define_args(Rest).


find_define([]) ->
    [];

find_define([{atom, _, define} = Token | Rest]) ->
    {DefineTokens, Tail} = find_dot(Rest),
    [Token] ++ replace_macro_args(DefineTokens) ++ Tail;

find_define([{white_space, _, _} = Token | Rest]) ->
    [Token] ++ find_define(Rest);

find_define(Tokens) ->
    Tokens.


find_dot([]) ->
    erlang:error({error, unterminated_define});

find_dot([{dot, _} = Dot | Rest]) ->
    {[Dot], Rest};

find_dot([Token | Rest]) ->
    {Define, Tail} = find_dot(Rest),
    {[Token | Define], Tail}.


unwind_close_paren([{')', _} = Close | Rest]) ->
    {Rest, [Close]};

unwind_close_paren([{white_space, _, _} = WS | Rest]) ->
    {Tail, Close} = unwind_close_paren(Rest),
    {Tail, [WS | Close]};

unwind_close_paren(_) ->
    erlang:error({error, unterminated_define}).


parse_forms([], Acc) ->
    lists:reverse(Acc);

parse_forms([{eof, _}], Acc) ->
    lists:reverse(Acc);

parse_forms(Tokens, Acc) ->
    case take_form(Tokens) of
        {no_form, Discard} ->
            Anno = element(2, Discard),
            ?EMILIO_REPORT(Anno, 901, "Invalid form at end of file"),
            lists:reverse([Discard | Acc]);
        {Form, Rest} ->
            case emilio_erl_parse:parse_form(Form) of
                {ok, AbstractForm} ->
                    parse_forms(Rest, [AbstractForm | Acc]);
                {error, {Loc, emilio_erl_parse, Message}} ->
                    Anno = case Loc of
                        {Line, Col} ->
                            [{line, Line}, {column, Col}];
                        _ ->
                            [{line, 0}, {column, 0}]
                    end,
                    ?EMILIO_REPORT(Anno, 902, Message),
                    Start = element(2, hd(Form)),
                    End = element(2, lists:last(Form)),
                    parse_forms(Rest, [{discard, Start, End} | Acc])
            end
    end.


take_form(Tokens) ->
    IsNotDot = fun(Token) -> element(1, Token) /= dot end,
    case lists:splitwith(IsNotDot, Tokens) of
        {Form, [Dot | Rest]} ->
            {Form ++ [Dot], Rest};
        {_, []} ->
            Start = element(2, hd(Tokens)),
            End = element(2, lists:last(Tokens)),
            {no_form, {discard, Start, End}}
    end.


linearize({attribute, _Anno, module, _Name} = Elem) ->
    [Elem];

linearize({attribute, _Anno, behavior, _Name} = Elem) ->
    [Elem];

linearize({attribute, _Anno, beahviour, _Name} = Elem) ->
    [Elem];

linearize({attribute, Anno, export, FAList}) ->
    Exports = [{export, Aa, F, A} || {Aa, F, A} <- FAList],
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
    LinearArgs = lists:flatmap(fun(Arg) ->
        linearize_expr(Arg, 0)
    end, Args),
    [{attribute, Anno, define, length(Args)}] ++ LinearArgs;

linearize({attribute, Anno, record, {Name, Fields}}) ->
    LinearFields = lists:flatmap(fun linearize/1, Fields),
    [{attribute, Anno, record, Name, length(Fields)} | LinearFields];

linearize({record_field, Anno, Name}) ->
    [{record_field, Anno, Name}];

linearize({record_field, Anno, Name, Initializer}) ->
    LinearInit = linearize_expr(Initializer, 0),
    [{record_field, Anno, Name}] ++ [{record_field_init, Anno}] ++ LinearInit;

linearize({typed_record_field, Field, Type}) ->
    LinearField = linearize(Field),
    LinearType = linearize_type(Type),
    LinearField
            ++ [{record_field_type, element(2, hd(LinearType))}]
            ++ LinearType;

linearize({attribute, Anno, TypeAttr, {Name, Type, Vars}})
        when TypeAttr == 'type'; TypeAttr == 'opaque' ->
    LinearVars = lists:flatmap(fun(Var) ->
        linearize_expr(Var, 0)
    end, Vars),
    [{attribute, Anno, TypeAttr, Name, length(Vars)}]
            ++ LinearVars
            ++ linearize_type(Type);

linearize({attribute, Anno, SpecAttr, {{Name, Arity}, TypeList}})
        when SpecAttr == 'spec'; SpecAttr == 'callback' ->
    LinearTypes = lists:flatmap(fun linearize_type/1, TypeList),
    [{attribute, Anno, SpecAttr, {Name, Arity}, length(TypeList)}]
            ++ LinearTypes;

linearize({attribute, Anno, SpecAttr, {{Mod, Name, Arity}, TypeList}})
        when SpecAttr == 'spect'; SpecAttr == 'callback' ->
    LinearTypes = lists:flatmap(fun linearize_type/1, TypeList),
    [{attribute, Anno, SpecAttr, {Mod, Name, Arity}, length(TypeList)}]
            ++ LinearTypes;

linearize({attribute, _Anno, _Name, _Value} = Elem) ->
    [Elem];

linearize({function, Anno, Name, Arity, Clauses}) ->
    LinearClauses =
            linearize_clause_list(function_clause, Anno, Clauses, 0),
    [{function, Anno, Name, Arity, length(Clauses)}] ++ LinearClauses;

linearize({discard, _, _} = Elem) ->
    [Elem].


linearize_type({ann_type, Anno, [AfAnno | SubTypes]}) ->
    reposition([{ann_type, Anno, length(SubTypes)}]
            ++ linearize_type(AfAnno)
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({type, Anno, binary, IntParams}) ->
    [{type, Anno, binary, length(IntParams)}]
            ++ lists:flatmap(fun linearize_type/1, IntParams);

linearize_type({type, Anno, nil, []}) ->
    [{type, Anno, nil}];

linearize_type({type, Anno, 'fun', []}) ->
    [{type, Anno, 'fun'}];

linearize_type({type, Anno, 'fun', [{type, Anno, 'any'}, ReturnType]}) ->
    [{type, Anno, 'fun'}]
            ++ linearize_type(ReturnType);

linearize_type({type, Anno, 'fun', FunctionType}) ->
    [{type, _, product, ArgTypes}, ReturnType] = FunctionType,
    reposition([{type, Anno, 'fun', length(ArgTypes)}]
            ++ lists:flatmap(fun linearize_type/1, ArgTypes)
            ++ linearize_type(ReturnType));

linearize_type({type, Anno, bounded_fun, [FunType, Constraints]}) ->
    reposition([{type, Anno, bounded_fun, length(Constraints)}]
            ++ linearize_type(FunType)
            ++ lists:flatmap(fun linearize_type/1, Constraints));

linearize_type({type, Anno, constraint, [{_, _, is_subtype}, [Var, Type]]}) ->
    reposition([{type, Anno, constraint, is_subtype}]
            ++ linearize_type(Var)
            ++ linearize_type(Type));

linearize_type({type, Anno, range, IntTypes}) ->
    reposition([{type, Anno, range, length(IntTypes)}]
            ++ lists:flatmap(fun linearize_type/1, IntTypes));

linearize_type({type, Anno, map, any}) ->
    [{type, Anno, map}];

linearize_type({type, Anno, map, AssocTypes}) ->
    reposition([{type, Anno, map, length(AssocTypes)}]
            ++ lists:flatmap(fun linearize_type/1, AssocTypes));

linearize_type({type, Anno, map_field_assoc, SubTypes}) ->
    reposition([{type, Anno, map_field_assoc, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({type, Anno, map_field_exact, SubTypes}) ->
    reposition([{type, Anno, map_field_exact, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({type, Anno, 'record', [Name | FieldTypes]}) ->
    reposition([{type, Anno, 'record', Name, length(FieldTypes)}]
            ++ linearize_type(Name)
            ++ lists:flatmap(fun linearize_type/1, FieldTypes));

linearize_type({type, Anno, field_type, [Name, Type]}) ->
    reposition([{type, Anno, field_type}]
            ++ linearize_type(Name)
            ++ linearize_type(Type));

linearize_type({remote_type, Anno, [Mod, TypeName, ArgTypes]}) ->
    reposition([{remote_type, Anno, length(ArgTypes)}]
            ++ linearize_type(Mod)
            ++ linearize_type(TypeName)
            ++ lists:flatmap(fun linearize_type/1, ArgTypes));

linearize_type({type, Anno, tuple, any}) ->
    [{type, Anno, tuple}];

linearize_type({type, Anno, tuple, SubTypes}) ->
    reposition([{type, Anno, tupl, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({type, Anno, union, SubTypes}) ->
    reposition([{type, Anno, union, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({user_type, Anno, TypeName, SubTypes}) ->
    reposition([{user_type, Anno, TypeName, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type({type, Anno, TypeName, SubTypes}) ->
    reposition([{type, Anno, TypeName, length(SubTypes)}]
            ++ lists:flatmap(fun linearize_type/1, SubTypes));

linearize_type(Else) ->
    linearize_expr(Else, 0).


linearize_expr({atom, _Anno, _Atom} = Elem, _Depth) ->
    [Elem];

linearize_expr({char, _Anno, _Char} = Elem, _Depth) ->
    [Elem];

linearize_expr({float, _Anno, _Float} = Elem, _Depth) ->
    [Elem];

linearize_expr({integer, _Anno, _Int} = Elem, _Dept) ->
    [Elem];

linearize_expr({string, _Anno, _String} = Elem, _Depth) ->
    [Elem];

linearize_expr({match, Anno, Pattern, Match}, Depth) ->
    Left = linearize_expr(Pattern, Depth),
    Right = linearize_expr(Match, Depth),
    Left ++ [set_depth({match, Anno}, Depth)] ++ Right;

linearize_expr({var, _Anno, _Atom} = Elem, _Depth) ->
    [Elem];

linearize_expr({macro, _Anno, _Atom} = Elem, _Depth) ->
    [Elem];

linearize_expr({tuple, Anno, Elems}, Depth) ->
    [set_depth({tuple, Anno, length(Elems)}, Depth)]
            ++ linearize_expr_list(Anno, Elems, Depth);

linearize_expr({nil, Anno}, _Depth) ->
    [{nil, Anno}];

linearize_expr({cons, Anno, Head, Tail}, Depth) ->
    LinearHead = linearize_expr(Head, Depth + 1),
    LinearTail = linearize_expr(Tail, Depth),
    reposition([set_depth({cons, Anno}, Depth)]
            ++ LinearHead
            ++ LinearTail);

linearize_expr({bin, Anno, Elems}, Depth) ->
    [set_depth({bin, Anno, length(Elems)}, Depth)]
            ++ linearize_expr_list(Anno, Elems, Depth);

linearize_expr({bin_element, Anno, Expr, Size, TSL}, Depth) ->
    Linearized = linearize_expr(Expr, Depth),
    EndAnno0 = element(2, lists:last(Linearized)),
    EndAnno1 = lists:keydelete(ref, 1, EndAnno0),
    reposition([set_depth({bin_element, Anno}, Depth)]
            ++ Linearized
            ++ [{bin_size, EndAnno1, Size}, {bin_tsl, EndAnno1, TSL}]);

linearize_expr({op, Anno, Op, Left, Right}, Depth) ->
    LinearLeft = linearize_expr(Left, Depth),
    LinearRight = linearize_expr(Right, Depth),
    LinearLeft ++ [set_depth({op2, Anno, Op}, Depth)] ++ LinearRight;

linearize_expr({op, Anno, Op, Right}, Depth) ->
    LinearRight = linearize_expr(Right, Depth),
    [set_depth({op1, Anno, Op}, Depth)] ++ LinearRight;

linearize_expr({record, Anno, Name, Fields}, Depth) ->
    LinearFields = linearize_expr_list(Anno, Fields, Depth),
    reposition([set_depth({record, Anno, Name, length(Fields)}, Depth)]
            ++ LinearFields);

linearize_expr({record, Anno, Expr, Name, Fields}, Depth) ->
    LinearExpr = linearize_expr(Expr, Depth),
    reposition([set_depth({record_update, Anno, Name, length(Fields)}, Depth)]
            ++ LinearExpr
            ++ linearize_expr_list(Anno, Fields, Depth));

linearize_expr({record_index, _Anno, _Name, _Field} = Elem, _Depth) ->
    [Elem];

linearize_expr({record_field, Anno, Name, Expr}, Depth) ->
    LinearExpr = linearize_expr(Expr, Depth),
    reposition([set_depth({record_field, Anno, Name}, Depth)] ++ LinearExpr);

linearize_expr({record_field, Anno, Expr, Name, Field}, Depth) ->
    LinearExpr = linearize_expr(Expr, Depth),
    reposition([set_depth({record_field, Anno}, Depth)]
            ++ LinearExpr
            ++ [{atom, Anno, Name}]
            ++ [Field]);

linearize_expr({map, Anno, Assocs}, Depth) ->
    [set_depth({map, Anno, length(Assocs)}, Depth)]
            ++ linearize_expr_list(Anno, Assocs, Depth);

linearize_expr({map, Anno, Expr, Assocs}, Depth) ->
    LinearExpr = linearize_expr(Expr, Depth),
    reposition([set_depth({map_update, Anno, length(Assocs)}, Depth)]
            ++ LinearExpr
            ++ linearize_expr_list(Anno, Assocs, Depth));

linearize_expr({map_field_assoc, Anno, Key, Val}, Depth) ->
    LinearKey = linearize_expr(Key, Depth),
    LinearVal = linearize_expr(Val, Depth),
    reposition([set_depth({map_field_assoc, Anno}, Depth)]
            ++ LinearKey
            ++ LinearVal);

linearize_expr({map_field_exact, Anno, Key, Val}, Depth) ->
    LinearKey = linearize_expr(Key, Depth),
    LinearVal = linearize_expr(Val, Depth),
    reposition([set_depth({map_field_exact, Anno}, Depth)]
            ++ LinearKey
            ++ LinearVal);

linearize_expr({'catch', Anno, Expr}, Depth) ->
    LinearExpr = linearize_expr(Expr, Depth),
    [set_depth({'catch_expr', Anno}, Depth)] ++ LinearExpr;

linearize_expr({call, Anno, {remote, Anno, Mod, Fun}, Args}, Depth) ->
    LinearMF = linearize_expr_list(Anno, [Mod, Fun], Depth),
    LinearArgs = linearize_expr_list(Anno, Args, Depth),
    reposition([set_depth({call_remote, Anno, length(Args)}, Depth)]
            ++ LinearMF
            ++ LinearArgs);

linearize_expr({call, Anno, Fun, Args}, Depth) ->
    LinearFun = linearize_expr_list(Anno, [Fun], Depth),
    LinearArgs = linearize_expr_list(Anno, Args, Depth),
    reposition([set_depth({call, Anno, length(Args)}, Depth)]
            ++ LinearFun
            ++ LinearArgs);

linearize_expr({lc, Anno, Template, Qualifiers}, Depth) ->
    LinearTemplate = linearize_expr(Template, Depth),
    reposition([set_depth({lc, Anno, length(Qualifiers)}, Depth)]
            ++ LinearTemplate
            ++ linearize_expr_list(Anno, Qualifiers, Depth));

linearize_expr({bc, Anno, Template, Qualifiers}, Depth) ->
    LinearTemplate = linearize_expr(Template, Depth),
    reposition([set_depth({bc, Anno, length(Qualifiers)}, Depth)]
            ++ LinearTemplate
            ++ linearize_expr_list(Anno, Qualifiers, Depth));

linearize_expr({generate, Anno, Pattern, Expr}, Depth) ->
    LinearPattern = linearize_expr(Pattern, Depth),
    LinearExpr = linearize_expr(Expr, Depth),
    LinearPattern ++ [set_depth({generate, Anno}, Depth)] ++ LinearExpr;

linearize_expr({b_generate, Anno, Pattern, Expr}, Depth) ->
    LinearPattern = linearize_expr(Pattern, Depth),
    LinearExpr = linearize_expr(Expr, Depth),
    LinearPattern ++ [set_depth({b_generate, Anno}, Depth)] ++ LinearExpr;

linearize_expr({block, Anno, Body}, Depth) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    [set_depth({block_start, StartAnno}, Depth)]
            ++ linearize_expr_list(Anno, Body, Depth)
            ++ [set_depth(End, Depth)];

linearize_expr({'if', Anno, Clauses}, Depth) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses =
            linearize_clause_list(if_clause, StartAnno, Clauses, Depth),
    [set_depth({'if', StartAnno, length(Clauses)}, Depth)]
            ++ LinearClauses
            ++ [set_depth(End, Depth)];

linearize_expr({'case', Anno, Expr, Clauses}, Depth) ->
    [StartAnno, {'end', End}, {'of', Of}] = split_anno(Anno),
    LinearExpr = linearize_expr(Expr, Depth),
    LinearClauses =
            linearize_clause_list(case_clause, StartAnno, Clauses, Depth),
    [set_depth({'case', StartAnno, length(Clauses)}, Depth)]
            ++ set_expr_span(LinearExpr, emilio_anno:ref(StartAnno))
            ++ [set_depth(Of, Depth)]
            ++ LinearClauses
            ++ [set_depth(End, Depth)];

linearize_expr({'try', Anno, Body, Cases, Catches, After}, Depth) ->
    [StartAnno | RestAnnos] = split_anno(Anno),
    OfToken = case lists:keyfind('of', 1, RestAnnos) of
        {'of', OT} -> [set_depth(OT, Depth)];
        false -> []
    end,
    CatchToken = case lists:keyfind('catch', 1, RestAnnos) of
        {'catch', CT} -> [set_depth(CT, Depth)];
        false -> []
    end,
    AfterToken = case lists:keyfind('after', 1, RestAnnos) of
        {'after', AT} -> [set_depth(AT, Depth)];
        false -> []
    end,
    {'end', End} = lists:keyfind('end', 1, RestAnnos),
    LinearBody = if Body == [] -> []; true ->
        linearize_expr_list(Anno, Body, Depth)
    end,
    LinearCases = if Cases == [] -> []; true ->
        LC = linearize_clause_list(try_case_clause, StartAnno, Cases, Depth),
        OfToken ++ LC
    end,
    LinearCatches = if Catches == [] -> []; true ->
        Type = try_catch_clause,
        LT = linearize_clause_list(Type, StartAnno, Catches, Depth),
        CatchToken ++ LT
    end,
    LinearAfter = if After == [] -> []; true ->
        AfterToken ++ linearize_expr_list(Anno, After, Depth)
    end,
    [set_depth({
        'try',
        StartAnno,
        length(Body),
        length(Cases),
        length(Catches),
        length(After)
    }, Depth)]
            ++ set_expr_span(LinearBody, emilio_anno:ref(StartAnno))
            ++ LinearCases
            ++ LinearCatches
            ++ LinearAfter
            ++ [set_depth(End, Depth)];

linearize_expr({'receive', Anno, Clauses}, Depth) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses =
            linearize_clause_list(receive_clause, StartAnno, Clauses, Depth),
    [set_depth({'receive', StartAnno, length(Clauses)}, Depth)]
            ++ LinearClauses
            ++ [set_depth(End, Depth)];

linearize_expr({'receive', Anno, Clauses, Timeout, After}, Depth) ->
    [StartAnno, {'after', AfterToken}, {'end', End}] = split_anno(Anno),
    LinearClauses =
            linearize_clause_list(receive_clause, StartAnno, Clauses, Depth),
    LinearTimeout = linearize_expr(Timeout, Depth),
    LinearAfter = linearize_expr_list(Anno, After, Depth),
    [set_depth({'receive', StartAnno, length(Clauses)}, Depth)]
            ++ LinearClauses
            ++ [set_depth(AfterToken, Depth)]
            ++ LinearTimeout
            ++ set_expr_span(LinearAfter, emilio_anno:ref(StartAnno))
            ++ [set_depth(End, Depth)];

linearize_expr({'fun', _Anno, {function, _F, _A}} = Elem, _Depth) ->
    [Elem];

linearize_expr({'fun', _Ann0, {function, _M, _F, _A}} = Elem, _Depth) ->
    [Elem];

linearize_expr({'fun', Anno0, {clauses, Clauses}}, Depth) ->
    [StartAnno, {'end', End}] = split_anno(Anno0),
    LinearClauses =
            linearize_clause_list(fun_clause, StartAnno, Clauses, Depth),
    [set_depth({'fun', StartAnno, length(Clauses)}, Depth)]
            ++ LinearClauses
            ++ [set_depth(End, Depth)];

linearize_expr({named_fun, Anno, Name, Clauses}, Depth) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses =
            linearize_clause_list(named_fun_clause, StartAnno, Clauses, Depth),
    [set_depth({named_fun, StartAnno, Name, length(Clauses)}, Depth)]
            ++ LinearClauses
            ++ [set_depth(End, Depth)].


linearize_clause(Type, SourceAnno,
        {clause, Anno, Patterns, Guards, Body}, Depth) ->
    NewAnno = emilio_anno:copy_ref(SourceAnno, Anno),
    linearize_clause(Type, {clause, NewAnno, Patterns, Guards, Body}, Depth).


linearize_clause(Type, {clause, Anno, Patterns, Guards, Body}, Depth) ->
    LinearPatterns = if length(Patterns) == 0 -> []; true ->
        linearize_expr_list(Anno, Patterns, Depth)
    end,
    LinearGuards = linearize_guards(Anno, Guards, Depth),
    LinearBody = linearize_expr_list(Anno, Body, Depth),
    ClauseToken =
            set_depth({Type, Anno, length(Patterns), length(Guards)}, Depth),
    reposition([ClauseToken]
            ++ LinearPatterns
            ++ LinearGuards
            ++ set_expr_span(LinearBody, emilio_anno:ref(Anno))).


linearize_guards(_, [], _) ->
    [];

linearize_guards(Anno, [{'when', WhenAnno} | Guards], Depth) ->
    NewWhenAnno = emilio_anno:copy_ref(Anno, WhenAnno),
    [set_depth({'when', NewWhenAnno}, Depth)]
            ++ linearize_guards(Anno, Guards, Depth);

linearize_guards(ParentAnno, Guards, Depth) ->
    lists:flatmap(fun(GuardExprs) ->
        LinearExprs = linearize_expr_list(ParentAnno, GuardExprs, Depth),
        Anno = element(2, hd(LinearExprs)),
        NewAnno = lists:keydelete(depth, 1, Anno),
        GuardToken = {guard, NewAnno, length(GuardExprs)},
        [set_depth(GuardToken, Depth)] ++ LinearExprs
    end, Guards).


linearize_expr_list(_, [], _) ->
    [];

linearize_expr_list(Anno, Exprs, Depth) ->
    SpanRef = erlang:make_ref(),
    LinearExprs = lists:map(fun(Expr) ->
        set_logical_span(linearize_expr(Expr, Depth + 1), SpanRef)
    end, Exprs),
    ParentRef = emilio_anno:ref(Anno),
    set_logical_span(lists:flatten(LinearExprs), ParentRef, SpanRef).


linearize_clause_list(Type, Anno, Clauses, Depth) ->
    LinearClauses = lists:map(fun(Clause) ->
        linearize_clause(Type, Anno, Clause, Depth + 1)
    end, Clauses),
    lists:flatten(LinearClauses).


reposition([Token]) ->
    [Token];

reposition([Curr, Next | Rest] = Tokens) ->
    CurrLoc = emilio_anno:lc(Curr),
    NextLoc = emilio_anno:lc(Next),
    case CurrLoc < NextLoc of
        true ->
            Tokens;
        false ->
            NewAnno = emilio_anno:set_location(Curr, Next),
            [setelement(2, Curr, NewAnno), Next | Rest]
    end.


set_logical_span(Tokens, ParentRef) ->
    set_span(Tokens, ParentRef, erlang:make_ref(), logical).


set_logical_span(Tokens, ParentRef, Ref) ->
    set_span(Tokens, ParentRef, Ref, logical).


set_expr_span(Tokens, ParentRef) ->
    set_span(Tokens, ParentRef, erlang:make_ref(), expr).


set_span(Tokens, ParentRef, Ref, Type)
        when is_list(Tokens), is_reference(ParentRef) ->
    SpanAnno = [{ref, Ref}, {parent_ref, ParentRef}],
    FirstAnno = element(2, hd(Tokens)),
    LastAnno = element(2, lists:last(Tokens)),
    Head = {span_start, emilio_anno:set_location(SpanAnno, FirstAnno), Type},
    Tail = {span_end, emilio_anno:set_location(SpanAnno, LastAnno), Type},
    [Head] ++ Tokens ++ [Tail].


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


coalesce_whitespace([]) ->
    [];

coalesce_whitespace([{white_space, Anno, Text} = Token | Rest]) ->
    case lists:last(Text) of
        $\n ->
            [Token] ++ coalesce_whitespace(Rest);
        _ ->
            case Rest of
                [{white_space, _, Append} | Tail] ->
                    NewToken = {white_space, Anno, Text ++ Append},
                    [NewToken] ++ coalesce_whitespace(Tail);
                _ ->
                    [Token] ++ coalesce_whitespace(Rest)
            end
    end;

coalesce_whitespace([Token | Rest]) ->
    [Token] ++ coalesce_whitespace(Rest).


reinsert_tokens([], Nodes) ->
    Nodes;

reinsert_tokens(Tokens, []) ->
    Tokens;

reinsert_tokens([Token | RestTokens] = Tokens, [Node | RestNodes] = Nodes) ->
    TokLoc = get_location(Token),
    NodeLoc = get_location(Node),
    IsDiscardNode = element(1, Node) == discard,
    case {TokLoc, NodeLoc} of
        _ when TokLoc < NodeLoc ->
            Rest = reinsert_tokens(RestTokens, Nodes),
            [Token | Rest];
        _ when TokLoc >= NodeLoc andalso IsDiscardNode ->
            % A discard node comes from when we failed to
            % parse a form. This logic makes sure that we
            % remove all tokens from the form so that we
            % don't attempt to process raw tokens from
            % erl_scan.
            NewTokens = discard_tokens(Tokens, element(3, Node)),
            reinsert_tokens(NewTokens, RestNodes);
        _ when TokLoc > NodeLoc ->
            Rest = reinsert_tokens(Tokens, RestNodes),
            [Node | Rest];
        _ when TokLoc == NodeLoc ->
            reinsert_tokens(RestTokens, Nodes)
    end.


discard_tokens([], _) ->
    [];

discard_tokens([Token | RestTokens] = Tokens, End) ->
    TokLoc = emilio_anno:lc(Token),
    EndLoc = emilio_anno:lc(End),
    case TokLoc =< EndLoc of
        true ->
            discard_tokens(RestTokens, End);
        false ->
            discard_leading_whitespace(Tokens)
    end.


discard_leading_whitespace([]) ->
    [];

discard_leading_whitespace([{white_space, _, _} | RestTokens]) ->
    discard_leading_whitespace(RestTokens);

discard_leading_whitespace(Tokens) ->
    Tokens.


detextify([]) ->
    [];

detextify([Token | RestTokens]) ->
    Anno = element(2, Token),
    NewAnno = lists:keydelete(text, 1, Anno),
    NewTok = setelement(2, Token, NewAnno),
    [NewTok] ++ detextify(RestTokens).


split_anno(Anno) ->
    % These keys need to stay sorted so that we can
    % reliably pattern match the response of this
    % function.
    Keys = ['after', 'catch', 'end', 'of'],
    {ref, Ref} = lists:keyfind(ref, 1, Anno),
    {Tokens, BaseAnno} = lists:mapfoldl(fun(Key, AccAnno0) ->
        case lists:keytake(Key, 1, AccAnno0) of
            {value, {Key, Token}, AccAnno1} ->
                TokenAnno = element(2, Token),
                NewAnno = lists:keystore(ref, 1, TokenAnno, {ref, Ref}),
                NewToken = setelement(2, Token, NewAnno),
                {[{Key, NewToken}], AccAnno1};
            false ->
                {[], AccAnno0}
        end
    end, Anno, Keys),
    [BaseAnno] ++ lists:flatten(Tokens).


set_depth(Term, Depth)
        when is_tuple(Term), size(Term) >= 2, is_integer(Depth) ->
    Anno = element(2, Term),
    case lists:keyfind(depth, 1, Anno) of
        {depth, Depth} ->
            Term;
        false ->
            NewAnno = lists:keystore(depth, 1, Anno, {depth, Depth}),
            setelement(2, Term, NewAnno);
        _ ->
            erlang:error({error, {bad_depth, Term, Depth}})
    end.


get_location(Term) when is_tuple(Term), size(Term) >= 2 ->
    Anno = element(2, Term),
    {line, Line} = lists:keyfind(line, 1, Anno),
    {column, Column} = lists:keyfind(column, 1, Anno),
    {Line, Column}.


is_code({white_space, _, _}) -> false;
is_code({comment, _, _}) -> false;
is_code(_) -> true.


stringify_tokens([]) ->
    [];

stringify_tokens(Tokens) ->
    % Convert arg tokens to a string
    TextList = lists:foldl(fun
        ({N, _}, Acc) -> [atom_to_list(N) | Acc];
        ({_, _, V}, Acc) -> [io_lib:format("~p", [V]) | Acc]
    end, [], Tokens),
    Text = lists:flatten(lists:reverse(TextList)),
    [{string, element(2, hd(Tokens)), Text}].
