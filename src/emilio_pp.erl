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
    format_error/2
]).


-include("emilio.hrl").


-define(SCAN_OPTS, [text, return]).


file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    {ok, AllTokens, _} = emilio_erl_scan:string(
            binary_to_list(Data),
            {1, 1},
            ?SCAN_OPTS
        ),
    Reverted = revert_annos(AllTokens),
    MacroedTokens = macroize(Reverted),
    {CodeTokens, _NonCodeTokens} = split_code(MacroedTokens),
    Forms = parse_forms(CodeTokens, []),
    Linearized = lists:flatmap(fun linearize/1, Forms),
    Rewhitespaced = rewhitespace(MacroedTokens),
    Reinserted = reinsert_tokens(Rewhitespaced, Linearized),
    DeTexted = detextify(Reinserted),
    group_lines(DeTexted).


format_error(901, Arg) ->
    io_lib:format("Unable to parse form: ~s", [Arg]).


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


revert_annos([]) ->
    [];

revert_annos([Token | Rest]) ->
    Anno = element(2, Token),
    {value, {location, {Line, Col}}, RestAnno} =
            lists:keytake(location, 1, Anno),
    NewAnno = RestAnno ++ [{line, Line}, {column, Col}],
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
            [NewToken] ++ macroize(RestTokens);
        not_a_macro ->
            [MacroToken] ++ macroize(Rest)
    end;

macroize([Token | Rest]) ->
    [Token | macroize(Rest)].


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
                    ?EMILIO_REPORT(Anno, 901, Message),
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
    LinearInit = linearize_expr(Initializer),
    [{record_field, Anno, Name}] ++ [{record_field_init, Anno}] ++ LinearInit;

linearize({typed_record_field, Field, Type}) ->
    LinearField = linearize(Field),
    LinearType = linearize_type(Type),
    LinearField ++ [{record_field_type, element(2, Field)}] ++ LinearType;

linearize({attribute, Anno, TypeAttr, {Name, Type, Vars}})
        when TypeAttr == 'type'; TypeAttr == 'opaque' ->
    LinearVars = lists:flatmap(fun linearize_expr/1, Vars),
    [{attribute, Anno, TypeAttr, Name, length(Vars)}]
            ++ LinearVars
            ++ linearize_type(Type);

linearize({attribute, Anno, SpecAttr, {{Name, Arity}, TypeList}})
        when SpecAttr == 'spec'; SpecAttr == 'callback' ->
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
    NewAnno = Anno ++ [{ref, erlang:make_ref()}],
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(function_clause, NewAnno, C)
    end, Clauses),
    [{function, NewAnno, Name, Arity, length(Clauses)}] ++ LinearClauses;

linearize({discard, _, _} = Elem) ->
    [Elem].


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
    [{type, Anno, 'fun'}]
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
    Linearized = linearize_expr(Expr),
    EndAnno = element(2, lists:last(Linearized)),
    [{bin_element, Anno}]
            ++ Linearized
            ++ [{bin_size, EndAnno, Size}, {bin_tsl, EndAnno, TSL}];

linearize_expr({op, Anno, Op, Left, Right}) ->
    LinearLeft = linearize_expr(Left),
    LinearRight = linearize_expr(Right),
    LinearLeft ++ [{op2, Anno, Op}] ++ LinearRight;

linearize_expr({op, Anno, Op, Right}) ->
    LinearRight = linearize_expr(Right),
    [{op1, Anno, Op}] ++ LinearRight;

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
    CallAnno = set_earliest(Anno, LinearMod),
    [{call_remote, CallAnno, length(Args)}]
            ++ LinearMod
            ++ LinearFun
            ++ LinearArgs;

linearize_expr({call, Anno, Fun, Args}) ->
    LinearFun = linearize_expr(Fun),
    LinearArgs = lists:flatmap(fun linearize_expr/1, Args),
    CallAnno = set_earliest(Anno, LinearFun),
    [{call, CallAnno, length(Args)}] ++ LinearFun ++ LinearArgs;

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
    LinearPattern ++ [{generate, Anno}] ++ LinearExpr;

linearize_expr({b_generate, Anno, Pattern, Expr}) ->
    LinearPattern = linearize_expr(Pattern),
    LinearExpr = linearize_expr(Expr),
    LinearPattern ++ [{b_generate, Anno}] ++ LinearExpr;

linearize_expr({block, Anno, Body}) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearBody = lists:flatmap(fun linearize_expr/1, Body),
    [{block_start, StartAnno}] ++ LinearBody ++ [End];

linearize_expr({'if', Anno, Clauses}) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(if_clause, StartAnno, C)
    end, Clauses),
    [{'if', StartAnno, length(Clauses)}] ++ LinearClauses ++ [End];

linearize_expr({'case', Anno, Expr, Clauses}) ->
    [StartAnno, {'end', End}, {'of', Of}] = split_anno(Anno),
    LinearExpr = linearize_expr(Expr),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(case_clause, StartAnno, C)
    end, Clauses),
    [{'case', StartAnno, length(Clauses)}]
            ++ LinearExpr ++ [Of] ++ LinearClauses ++ [End];

linearize_expr({'try', Anno, Body, Cases, Catches, After}) ->
    [StartAnno | RestAnnos] = split_anno(Anno),
    OfToken = case lists:keyfind('of', 1, RestAnnos) of
        {'of', OT} -> [OT];
        false -> []
    end,
    CatchToken = case lists:keyfind('catch', 1, RestAnnos) of
        {'catch', CT} -> [CT];
        false -> []
    end,
    AfterToken = case lists:keyfind('after', 1, RestAnnos) of
        {'after', AT} -> [AT];
        false -> []
    end,
    {'end', End} = lists:keyfind('end', 1, RestAnnos),
    LinearBody = if Body == [] -> []; true ->
        lists:flatmap(fun linearize_expr/1, Body)
    end,
    LinearCases = if Cases == [] -> []; true ->
        OfToken ++ lists:flatmap(fun(C) ->
            linearize_clause(try_case_clause, StartAnno, C)
        end, Cases)
    end,
    LinearCatches = if Catches == [] -> []; true ->
        CatchToken ++ lists:flatmap(fun(C) ->
            linearize_clause(try_catch_clause, StartAnno, C)
        end, Catches)
    end,
    LinearAfter = if After == [] -> []; true ->
        AfterToken ++ lists:flatmap(fun linearize_expr/1, After)
    end,
    [{
        'try',
        StartAnno,
        length(Body),
        length(Cases),
        length(Catches),
        length(After)
    }]
            ++ LinearBody
            ++ LinearCases
            ++ LinearCatches
            ++ LinearAfter
            ++ [End];

linearize_expr({'receive', Anno, Clauses}) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(receive_clause, StartAnno, C)
    end, Clauses),
    [{'receive', StartAnno, length(Clauses)}] ++ LinearClauses ++ [End];

linearize_expr({'receive', Anno, Clauses, Timeout, After}) ->
    [StartAnno, {'after', AfterToken}, {'end', End}] = split_anno(Anno),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(receive_clause, StartAnno, C)
    end, Clauses),
    LinearTimeout = linearize_expr(Timeout),
    LinearAfter = lists:flatmap(fun linearize_expr/1, After),
    [{'receive', StartAnno, length(Clauses)}]
            ++ LinearClauses
            ++ [AfterToken]
            ++ LinearTimeout
            ++ LinearAfter
            ++ [End];

linearize_expr({'fun', _Anno, {function, _Name, _Arity}} = Elem) ->
    [Elem];

linearize_expr({'fun', _Ann0, {function, _Mod, _Name, _Arity}} = Elem) ->
    [Elem];

linearize_expr({'fun', Anno, {clauses, Clauses}}) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(fun_clause, StartAnno, C)
    end, Clauses),
    [{'fun', StartAnno, length(Clauses)}] ++ LinearClauses ++ [End];

linearize_expr({named_fun, Anno, Name, Clauses}) ->
    [StartAnno, {'end', End}] = split_anno(Anno),
    LinearClauses = lists:flatmap(fun(C) ->
        linearize_clause(named_fun_clause, StartAnno, C)
    end, Clauses),
    [{named_fun, StartAnno, Name, length(Clauses)}] ++ LinearClauses ++ [End].


linearize_clause(Type, SourceAnno, {clause, Anno, Patterns, Guards, Body}) ->
    NewAnno = emilio_anno:copy_ref(SourceAnno, Anno),
    linearize_clause(Type, {clause, NewAnno, Patterns, Guards, Body}).


linearize_clause(Type, {clause, Anno, Patterns, Guards, Body}) ->
    LinearPatterns = lists:flatmap(fun linearize_expr/1, Patterns),
    LinearGuards = linearize_guards(Anno, Guards),
    LinearBody = lists:flatmap(fun linearize_expr/1, Body),
    ClauseAnno = set_earliest(Anno, LinearPatterns),
    [{Type, ClauseAnno, length(Patterns), length(Guards)}]
            ++ LinearPatterns
            ++ LinearGuards
            ++ LinearBody.


linearize_guards(_, []) ->
    [];

linearize_guards(Anno, [{'when', WhenAnno} | Guards]) ->
    NewWhenAnno = emilio_anno:copy_ref(Anno, WhenAnno),
    [{'when', NewWhenAnno}] ++ linearize_guards(Guards);

% If statement guard clauses don't have a
% 'when' token.
linearize_guards(_Anno, Guards) ->
    linearize_guards(Guards).


linearize_guards(Guards) ->
    lists:flatmap(fun(GuardExprs) ->
        LinearExprs = lists:flatmap(fun linearize_expr/1, GuardExprs),
        [{guard, element(2, hd(LinearExprs)), length(GuardExprs)}]
                ++ LinearExprs
    end, Guards).


set_earliest(Anno, []) ->
    Anno;

set_earliest(Anno, [Token | _]) ->
    AnnoLoc = emilio_anno:lc(Anno),
    TokenLoc = emilio_anno:lc(Token),
    case TokenLoc < AnnoLoc of
        false ->
            Anno;
        true ->
            emilio_anno:set_location(Anno, TokenLoc)
    end.


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


group_lines([]) ->
    [];

group_lines([Tok | Rest]) ->
    group_lines(Rest, [Tok], []).


group_lines([], [], GroupAcc) ->
    lists:reverse(GroupAcc);

group_lines([], Group, GroupAcc) ->
    lists:reverse(GroupAcc, [lists:reverse(Group)]);

group_lines([Token | RestTokens], [G | _] = Group, GroupAcc) ->
    {TLine, _} = emilio_anno:lc(Token),
    {GLine, _} = emilio_anno:lc(G),
    case TLine > GLine of
        true ->
            NewGroupAcc = [lists:reverse(Group) | GroupAcc],
            group_lines(RestTokens, [Token], NewGroupAcc);
        false ->
            group_lines(RestTokens, [Token | Group], GroupAcc)
    end.


split_anno(Anno) ->
    % These keys need to stay sorted so that we can
    % reliably pattern match the response of this
    % function.
    Keys = ['after', 'catch', 'end', 'of'],
    Ref = erlang:make_ref(),
    {Tokens, BaseAnno} = lists:mapfoldl(fun(Key, AccAnno0) ->
        case lists:keytake(Key, 1, AccAnno0) of
            {value, {Key, Token}, AccAnno1} ->
                TokenAnno = element(2, Token),
                NewToken = setelement(2, Token, TokenAnno ++ [{ref, Ref}]),
                {[{Key, NewToken}], AccAnno1};
            false ->
                {[], AccAnno0}
        end
    end, Anno, Keys),
    [BaseAnno ++ [{ref, Ref}] | lists:flatten(Tokens)].


get_location(Term) when is_tuple(Term), size(Term) >= 2 ->
    Anno = element(2, Term),
    {line, Line} = lists:keyfind(line, 1, Anno),
    {column, Column} = lists:keyfind(column, 1, Anno),
    {Line, Column}.
