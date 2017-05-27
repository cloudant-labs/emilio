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

    report/4
]).


-record(ctx, {
    prev_lines,
    curr_line,
    rest_lines,

    prev_tokens,
    rest_tokens,

    logical_loc
}).


foreach_token(UserFun, Lines) ->
    traverse(Lines, [], wrap_user_fun(UserFun)).


foreach_line(UserFun, Lines) ->
    Wrapped = wrap_user_fun(UserFun),
    LineFun = fun(Loc, _Token, Ctx) ->
        if Ctx#ctx.prev_tokens /= [] -> ok; true ->
            Wrapped(Loc, Ctx#ctx.curr_line, Ctx)
        end
    end,
    foreach_token(LineFun, Lines).


prev_line(Ctx) ->
    case Ctx#ctx.prev_lines of
        [] -> undefined;
        [Line | _] -> Line
    end.

curr_line(Ctx) ->
    Ctx#ctx.curr_line.


next_line(Ctx) ->
    case Ctx#ctx.rest_lines of
        [] -> undefined;
        [Line | _] -> Line
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


report(Module, {Line, Col}, Code, Arg) ->
    Msg = Module:format_error(Code, Arg),
    Fmt = "~b(~b) : ~b : ~s~n",
    io:format(Fmt, [Line, Col, Code, Msg]).


wrap_user_fun(UserFun) when is_function(UserFun, 2) ->
    fun(Loc, Item, _) -> UserFun(Loc, Item) end;
wrap_user_fun(UserFun) when is_function(UserFun, 3) ->
    UserFun.
