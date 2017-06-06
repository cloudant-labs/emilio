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

    indent_level/1,
    is_blank_line/1
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


wrap_user_fun(UserFun) when is_function(UserFun, 2) ->
    fun(Anno, Item, _) -> UserFun(Anno, Item) end;
wrap_user_fun(UserFun) when is_function(UserFun, 3) ->
    UserFun.
