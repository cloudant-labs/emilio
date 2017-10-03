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

-module(emilio_check_indents_clause_bodies).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [122].


explain(122) ->
    "Indentation for clause bodies should increase by one level".


format_error(122, {Token, Change, [Expected]}) ->
    Name = element(1, Token),
    Fmt = "indentation change for ~s was ~b, not ~b",
    io_lib:format(Fmt, [Name, Change, Expected]);

format_error(122, {Token, Change, [Expect1, Expect2]}) ->
    Name = element(1, Token),
    Fmt = "indentation change for ~s was ~b, not ~b or ~b",
    io_lib:format(Fmt, [Name, Change, Expect1, Expect2]).


run(Lines) ->
    %io:format(standard_error, "~p~n", [Lines]),
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(Anno, Token, Ctx) ->
    try
        skip_ignored(Token),
        skip_reference_token(Token),
        skip_not_first_token(Token, Ctx),
        skip_split_operator(Token, Ctx),
        skip_multiline_string(Token, Ctx),
        skip_function_head(Token, Ctx),

        check_indent(Anno, Token, Ctx)
    catch throw:skip ->
        ok
    end.


skip_ignored(Token) ->
    Ignored = ['end', match, nil, op2, white_space, '}'],
    Name = element(1, Token),
    case lists:member(Name, Ignored) of
        true -> throw(skip);
        false -> ok
    end.


skip_reference_token(Token) ->
    case emilio_anno:ref(Token) of
        undefined -> ok;
        _ -> throw(skip)
    end.


skip_not_first_token(Token, Ctx) ->
    case is_first_token(Token, Ctx) of
        true -> ok;
        false -> throw(skip)
    end.


skip_split_operator(Token, Ctx) ->
    case is_op(Token) orelse is_op(prev_non_ws(Ctx)) of
        true -> throw(skip);
        false -> ok
    end.


skip_multiline_string(Token, Ctx) ->
    case is_string(Token) andalso is_string(prev_non_ws(Ctx)) of
        true -> throw(skip);
        false -> ok
    end.


skip_function_head(Token, Ctx) ->
    %% case is_function_head(Token, Ctx) of
    %%     true -> throw(skip);
    %%     false -> ok
    %% end.
    ok.


check_indent(Anno, Token, Ctx) ->
    case parent_token(Token, Ctx) of
        {PrevToken, PrevCtx} ->
            check_indent(Anno, Token, Ctx, PrevToken, PrevCtx);
        undefined ->
            ok
    end.


parent_token(Token, Ctx) ->
    InitDepth = case emilio_anno:depth(Token) of
        D when is_integer(D), D >= 0 -> D;
        _ -> 1000000
    end,
    case emilio_lib:iter_rev(Ctx, fun parent_iter/3, InitDepth) of
        {ok, Match} ->
            Match;
        _ ->
            undefined
    end.


parent_iter({dot, _}, _, _) ->
    {stop, {ok, undefined}};

parent_iter({'when', _}, _, Depth) ->
    {continue, Depth};

parent_iter({sep, Anno}, _, Depth) ->
    case emilio_anno:depth(Anno) of
        N when is_integer(N), N < Depth ->
            {continue, N};
        _ ->
            {continue, Depth}
    end;

parent_iter(Token, TokenCtx, Depth) ->
    case emilio_anno:depth(Token) of
        N when is_integer(N), N =< Depth ->
            case emilio_anno:ref(Token) of
                Ref when is_reference(Ref) ->
                    {stop, {ok, {Token, TokenCtx}}};
                _ ->
                    check_soft_parent(Token, TokenCtx, Depth)
            end;
        _ ->
            {continue, Depth}
    end.


check_soft_parent(Token, TokenCtx, Depth) ->
    Name = element(1, Token),
    SoftParents = [record, record_update, tuple, cons],
    case lists:member(Name, SoftParents) of
        true ->
            {stop, {ok, {Token, TokenCtx}}};
        false ->
            {continue, Depth}
    end.


check_indent(Anno, Token, Ctx, PrevToken, PrevCtx) ->
    IndentLevels = indent_levels(PrevToken),
    TokenLine = emilio_lib:curr_line(Ctx),
    TokenLevel = emilio_lib:indent_level(TokenLine),
    PrevLine = emilio_lib:curr_line(PrevCtx),
    PrevLevel = emilio_lib:indent_level(PrevLine),
    Diff = TokenLevel - PrevLevel,
    case lists:member(Diff, IndentLevels) of
        true ->
            ok;
        false ->
            ?EMILIO_REPORT(Anno, 122, {Token, Diff, IndentLevels})
    end.


indent_levels({'end', _}) ->
    [0];

indent_levels({cons, _}) ->
    [0, 1];

indent_levels(_) ->
    [1].


is_first_token(Token, Ctx) ->
    CurrLine = emilio_lib:curr_line(Ctx),
    IsWS = fun({white_space, _, _}) -> true; (_) -> false end,
    case lists:dropwhile(IsWS, CurrLine) of
        [Token | _] -> true;
        _ -> false
    end.


%%
%% is_function_head(Token, Ctx) ->
%%     IterToks = fun(Token, _, _) ->
%%         case element(1, Token) of
%%             function_clause -> {stop, true};
%%             '->'
%%

prev_non_ws(Ctx) ->
    IterWS = fun(Token, _, _) ->
        case element(1, Token) of
            white_space ->
                {continue, undefined};
            _ ->
                {stop, Token}
        end
    end,
    emilio_lib:iter_rev(Ctx, IterWS, undefined).


is_op({op2, _, _}) -> true;
is_op({match, _}) -> true;
is_op(_) -> false.


is_string({string, _, _}) -> true;
is_string(_) -> false.
