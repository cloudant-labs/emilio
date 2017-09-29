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

-module(emilio_lib).


-export([
    fold_tokens/3,
    foreach_token/2,
    foreach_line/2,

    prev_line/1,
    curr_line/1,
    next_line/1,

    prev_token/1,
    next_token/1,

    find_ref_fwd/2,
    find_ref_fwd/3,
    find_ref_rev/2,
    find_ref_rev/3,

    iter_fwd/3,
    iter_rev/3,

    indent_level/1,
    is_blank_line/1,

    group_lines/1,

    simplified_forms/1
]).


-record(ctx, {
    prev_lines,
    curr_line,
    rest_lines,

    prev_tokens,
    rest_tokens
}).


-define(DEFAULT_REF_NODES, [
    block_start,
    'if',
    'case',
    'try',
    'receive',
    'fun',
    'named_fun'
]).


fold_tokens(UserFun, Acc, [CurrLine | RestLines]) ->
    Ctx = #ctx{
        prev_lines = [],
        curr_line = CurrLine,
        rest_lines = RestLines,

        prev_tokens = [],
        rest_tokens = CurrLine
    },
    iter_fwd(Ctx, UserFun, Acc).


foreach_token(UserFun, Lines) ->
    traverse(Lines, [], wrap_user_fun(UserFun)).


foreach_line(UserFun, Lines) ->
    Wrapped = wrap_user_fun(UserFun),
    LineFun = fun(Anno, _Token, Ctx) ->
        if Ctx#ctx.prev_tokens /= [] -> ok; true ->
            Wrapped(Anno, Ctx#ctx.curr_line, Ctx)
        end
    end,
    foreach_token(LineFun, Lines).


prev_line(Ctx) ->
    case Ctx#ctx.prev_lines of
        [] ->
            undefined;
        [Line | Rest] ->
            NewCtx = Ctx#ctx{
                prev_lines = Rest,
                curr_line = Line,
                rest_lines = [Ctx#ctx.curr_line | Ctx#ctx.rest_lines],

                prev_tokens = [],
                rest_tokens = Line
            },
            {ok, Line, NewCtx}
    end.


curr_line(Ctx) ->
    Ctx#ctx.curr_line.


next_line(Ctx) ->
    case Ctx#ctx.rest_lines of
        [] ->
            undefined;
        [Line | Rest] ->
            NewCtx = Ctx#ctx{
                prev_lines = [Ctx#ctx.curr_line | Ctx#ctx.prev_lines],
                curr_line = Line,
                rest_lines = Rest,

                prev_tokens = [],
                rest_tokens = Line
            },
            {ok, Line, NewCtx}
    end.


prev_token(Ctx) ->
    case Ctx#ctx.prev_tokens of
        [Token | _] ->
            Token;
        [] ->
            case Ctx#ctx.prev_lines of
                [] -> undefined;
                [Line | _] -> lists:last(Line)
            end
    end.


next_token(Ctx) ->
    case Ctx#ctx.rest_tokens of
        [Token | _] ->
            Token;
        [] ->
            case Ctx#ctx.rest_lines of
                [] -> undefined;
                [Line | _] -> hd(Line)
            end
    end.


find_ref_fwd(Ctx, Ref) ->
    find_ref_fwd(Ctx, Ref, ?DEFAULT_REF_NODES).


find_ref_fwd(Ctx, Ref, Names) when is_reference(Ref), is_list(Names) ->
    find_ref(fun iter_fwd/3, Ctx, Ref, Names);

find_ref_fwd(Ctx, Ref, Name) when is_atom(Name) ->
    find_ref_fwd(Ctx, Ref, [Name]).


find_ref_rev(Ctx, Ref) ->
    find_ref_rev(Ctx, Ref, ?DEFAULT_REF_NODES).


find_ref_rev(Ctx, Ref, Names) when is_reference(Ref), is_list(Names) ->
    find_ref(fun iter_rev/3, Ctx, Ref, Names);

find_ref_rev(Ctx, Ref, Name) when is_atom(Name) ->
    find_ref_rev(Ctx, Ref, [Name]).


iter_fwd(#ctx{} = Ctx, Fun, Acc) ->
    case Ctx of
        #ctx{rest_tokens = [Token | Rest]} ->
            CurrCtx = Ctx#ctx{
                rest_tokens = Rest
            },
            case Fun(Token, CurrCtx, Acc) of
                {stop, NewAcc} ->
                    NewAcc;
                {continue, NewAcc} ->
                    NextCtx = Ctx#ctx{
                        prev_tokens = [Token | Ctx#ctx.prev_tokens],
                        rest_tokens = Rest
                    },
                    iter_fwd(NextCtx, Fun, NewAcc)
            end;
        #ctx{rest_tokens = [], rest_lines = [Line | Rest]} ->
            NextCtx = Ctx#ctx{
                prev_lines = [Ctx#ctx.curr_line | Ctx#ctx.prev_lines],
                curr_line = Line,
                rest_lines = Rest,

                prev_tokens = [],
                rest_tokens = Line
            },
            iter_fwd(NextCtx, Fun, Acc);
        #ctx{rest_tokens = [], rest_lines = []} ->
            Acc
    end.


iter_rev(Ctx, Fun, Acc) ->
    case Ctx of
        #ctx{prev_tokens = [Token | Rest]} ->
            CurrCtx = Ctx#ctx{
                prev_tokens = Rest
            },
            case Fun(Token, CurrCtx, Acc) of
                {stop, NewAcc} ->
                    NewAcc;
                {continue, NewAcc} ->
                    NextCtx = Ctx#ctx{
                        prev_tokens = Rest,
                        rest_tokens = [Token | Ctx#ctx.rest_tokens]
                    },
                    iter_rev(NextCtx, Fun, NewAcc)
            end;
        #ctx{prev_tokens = [], prev_lines = [Line | Rest]} ->
            NextCtx = Ctx#ctx{
                prev_lines = Rest,
                curr_line = Line,
                rest_lines = [Ctx#ctx.curr_line | Ctx#ctx.rest_lines],

                prev_tokens = lists:reverse(Line),
                rest_tokens = []
            },
            iter_rev(NextCtx, Fun, Acc);
        #ctx{prev_tokens = [], prev_lines = []} ->
            Acc
    end.


indent_level(Line) ->
    indent_level(Line, []).


indent_level([{white_space, _, "\n"} | _], Acc) ->
    indent_level(eol, Acc);

indent_level([{white_space, _, Text} | Rest], Acc) ->
    indent_level(Rest, [Text | Acc]);

indent_level(_, Text) ->
    Units = emilio_cfg:get(indentation_count),
    Spaces = length(lists:flatten(lists:reverse(Text))),
    Base = Spaces div Units,
    Base + if Spaces rem Units == 0 -> 0; true -> 1 end.


is_blank_line(Line) ->
    FiltFun = fun(Token) -> element(1, Token) /= white_space end,
    [] == lists:filter(FiltFun, Line).


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


simplified_forms(Lines) ->
    Groups0 = split_at_dots(Lines),
    Groups1 = lists:flatmap(fun split_group/1, Groups0),
    Groups2 = lists:flatmap(fun simplify_form/1, Groups1),
    group_new_lines(Groups2).


traverse([], _, _) ->
    ok;

traverse([Line | RestLines], PrevLines, UserFun) ->
    Ctx = #ctx{
        prev_lines = PrevLines,
        curr_line = Line,
        rest_lines = RestLines
    },
    traverse_line(Line, [], Ctx, UserFun),
    traverse(RestLines, [Line | PrevLines], UserFun).


traverse_line([], _, _Ctx, _UserFun) ->
    ok;

traverse_line([Token | RestTokens], PrevTokens, Ctx, UserFun) ->
    SubCtx = Ctx#ctx{
        prev_tokens = PrevTokens,
        rest_tokens = RestTokens
    },
    traverse_token(Token, SubCtx, UserFun),
    traverse_line(RestTokens, [Token | PrevTokens], Ctx, UserFun).


traverse_token(Token, Ctx, UserFun) ->
    UserFun(element(2, Token), Token, Ctx).


find_ref(IterFun, Ctx, Ref, Names) ->
    Fun = fun(Token, IterCtx, Acc) ->
        CurrRef = emilio_anno:ref(Token),
        Name = element(1, Token),
        case lists:member(Name, Names) of
            true when Ref == CurrRef ->
                {stop, {ok, Token, IterCtx}};
            _ ->
                {continue, Acc}
        end
    end,
    IterFun(Ctx, Fun, undefined).


split_at_dots([]) ->
    [];

% In case no tokens after the last dot
split_at_dots([[] | Rest]) ->
    split_at_dots(Rest);

split_at_dots([Line | Rest]) ->
    SplitFun = fun(Token) -> element(1, Token) /= dot end,
    case lists:splitwith(SplitFun, Line) of
        {Line, []} ->
            case split_at_dots(Rest) of
                [] ->
                    [[Line]];
                [Group | RestGroups] ->
                    [[Line | Group] | RestGroups]
            end;
        {Prefix, [Dot | Tail]} ->
            Groups = split_at_dots([Tail | Rest]),
            [[Prefix ++ [Dot]] | Groups]
    end.


split_group(Group) ->
    case lists:splitwith(fun has_no_code/1, Group) of
        {[], []} ->
            [];
        {NonCode, []} ->
            [NonCode];
        {[], RestGroup} ->
            [RestGroup];
        {NonCode, RestGroup} ->
            [NonCode, RestGroup]
    end.


has_no_code(Line) ->
    IsNotCode = fun
        ({white_space, _, _}) -> true;
        ({comment, _, _}) -> true;
        (_) -> false
    end,
    lists:all(IsNotCode, Line).


simplify_form([{white_space, Anno, _}]) ->
    [{new_line, Anno, 1}];

simplify_form(Form) ->
    {Comments, RestForm} = strip_comments(lists:flatten(Form)),
    if Comments == [] -> []; true ->
        group_comments(Comments)
    end ++ if RestForm == [] -> []; true ->
        [rewrite_form(RestForm)]
    end.


strip_comments([]) ->
    {[], []};

strip_comments([{comment, _, _} = C | Rest]) ->
    {Comments, Tail} = strip_comments(Rest),
    {[C | Comments], Tail};

strip_comments([{white_space, _, _} = WS | Rest]) ->
    {Comments, Tail} = strip_comments(Rest),
    {[WS | Comments], Tail};

strip_comments(Tail) ->
    {[], Tail}.


group_comments(Tokens) ->
    Lines = group_lines(Tokens),
    Simplified = lists:map(fun(Line) ->
        case Line of
            [{white_space, Anno, _}] ->
                {new_line, Anno, 1};
            _ ->
                {comment, element(2, hd(Line))}
        end
    end, Lines),
    NLGrouped = group_new_lines(Simplified),
    Condensed = lists:foldl(fun(Line, Acc) ->
        case Line of
            {new_line, Anno, Count} ->
                case Acc of
                    [{comment, _} | _] ->
                        % Account for the newline terminating
                        % the comment
                        [{new_line, Anno, Count + 1} | Acc];
                    _ ->
                        [{new_line, Anno, Count} | Acc]
                end;
            {comment, Anno} ->
                case Acc of
                    [{comment, _} | _] ->
                        Acc;
                    _ ->
                        [{comment, Anno} | Acc]
                end
        end
    end, [], NLGrouped),
    lists:reverse(Condensed).


rewrite_form([{'-', _}, {attribute, Anno, export} | Rest]) ->
    FiltFun = fun(Token) -> element(1, Token) == export end,
    Exports = lists:filter(FiltFun, Rest),
    {attribute, Anno, export, Exports};

rewrite_form([{'-', _}, Attr | _]) when element(1, Attr) == attribute ->
    Attr;

rewrite_form([Function | Body]) when element(1, Function) == function ->
    Clauses = split_function_clauses(Body),
    setelement(5, Function, Clauses).


split_function_clauses([]) ->
    [];

split_function_clauses([{function_clause, Anno, _, _} | Rest]) ->
    SplitFun = fun(Token) -> element(1, Token) /= function_clause end,
    {ClauseBody, NextClause} = lists:splitwith(SplitFun, Rest),
    RevBody = lists:reverse(ClauseBody),
    {Comments, _RevClauseBody} =  strip_comments(RevBody),
    Groups = group_comments(lists:reverse(Comments)),
    [{function_clause, Anno, Groups}] ++ split_function_clauses(NextClause).


group_new_lines([]) ->
    [];

group_new_lines([{new_line, Anno, C1} | Rest]) ->
    case Rest of
        [{new_line, _, C2} | Tail] ->
            group_new_lines([{new_line, Anno, C1 + C2} | Tail]);
        _ ->
            [{new_line, Anno, C1}] ++ group_new_lines(Rest)
    end;

group_new_lines([Form | Rest]) ->
    [Form] ++ group_new_lines(Rest).


wrap_user_fun(UserFun) when is_function(UserFun, 2) ->
    fun(Anno, Item, _) -> UserFun(Anno, Item) end;
wrap_user_fun(UserFun) when is_function(UserFun, 3) ->
    UserFun.
