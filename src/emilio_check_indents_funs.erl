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

-module(emilio_check_indents_funs).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(141, _) ->
    "inline fun does not start on call line";
format_error(142, _) ->
    "inline fun does not end on last line of call".


run(Lines) ->
    %io:format(standard_error, "~p~n", [Lines]),
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(_, {call_remote, _, Args} = Token, Ctx) when Args > 0 ->
    check_call(Ctx, Token);
check_token(_, {call, _, Args} = Token, Ctx) when Args > 0 ->
    check_call(Ctx, Token);
check_token(_, _, _) ->
    ok.


check_call(Ctx, Token) ->
    {ok, _ModFun, ArgCtx} = emilio_lib:collect_span(Ctx, Token),
    {ok, ArgTokens, _} = emilio_lib:collect_span(ArgCtx, Token),
    {Tree, _} = retree(ArgTokens),
    Args = lists:filter(fun is_list/1, Tree),

    {FirstLine, _} = emilio_anno:lc(Token),
    LastLine = last_line(lists:last(Args)),

    lists:foreach(fun(Arg) ->
        check_arg(Arg, FirstLine, LastLine)
    end, Args).


check_arg(Arg, FirstLine, LastLine) ->
    case Arg of
        [{'fun', FunAnno, Args} | RestArg] when is_integer(Args) ->
            {'end', EndAnno} = lists:last(RestArg),
            {FL, _} = emilio_anno:lc(FunAnno),
            if FL == FirstLine -> ok; true ->
                ?EMILIO_REPORT(FunAnno, 141)
            end,
            {EL, _} = emilio_anno:lc(EndAnno),
            if EL == LastLine -> ok; true ->
                ?EMILIO_REPORT(EndAnno, 142)
            end;
        _ ->
            ok
    end.


retree([{span_start, Anno, _} | RestTokens]) ->
    Ref = emilio_anno:ref(Anno),
    retree(RestTokens, Ref).


retree([{span_end, Anno, _} | RestTokens], Ref) ->
    Ref = emilio_anno:ref(Anno),
    {[], RestTokens};
retree([{span_start, _, _} | _] = Tokens, Ref) ->
    {SubSpan, RestTokens} = retree(Tokens),
    {RestSpan, LastTokens} = retree(RestTokens, Ref),
    {[SubSpan] ++ RestSpan, LastTokens};
retree([Token | RestTokens], Ref) ->
    {RestSpan, LastTokens} = retree(RestTokens, Ref),
    {[Token] ++ RestSpan, LastTokens}.


last_line([]) ->
    0;
last_line([Token]) when is_tuple(Token) ->
    {L, _} = emilio_anno:lc(Token),
    L;
last_line([Token | RestTokens]) when is_tuple(Token) ->
    {L, _} = emilio_anno:lc(Token),
    erlang:max(L, last_line(RestTokens));
last_line([Tokens | RestTokens]) when is_list(Tokens) ->
    erlang:max(last_line(Tokens), last_line(RestTokens)).
