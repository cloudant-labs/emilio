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

-module(emilio_check_indents_spans).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(125, _) ->
    "clause body line did not increase indent level by at least 1";
format_error(126, _) ->
    "expression span line did not increase indent level by at least 2".


run(Lines) ->
    emilio_lib:foreach_token(fun check_token/3, Lines).


% Between span_start and span_end we have these cases:
%
% 1) All tokens on the same line: do nothing
% 2) Tokens start on same line: all span lines should be at least two indents
% 3) Tokens start on next line: all span lines should be at least one indent


check_token(_, {span_start, Anno} = Token, Ctx) ->
    {Parent, ParentCtx} = find_prev_ref(Ctx, emilio_anno:parent_ref(Anno)),
    {SpanEnd, _} = find_next_ref(Ctx, emilio_anno:ref(Anno)),
    SpanLines = emilio_lib:collect_lines(Ctx, Token, SpanEnd),
    check_span(Parent, ParentCtx, Token, SpanLines);

check_token(_, _, _) ->
    ok.


check_span(Parent, ParentCtx, SpanStart, SpanLines) ->
    case same_line(Parent, SpanStart) of
        true ->
            case length(SpanLines) of
                1 ->
                    ok;
                N when N > 1 ->
                    ContLines = tl(SpanLines),
                    check_continuation_indents(ParentCtx, ContLines)
            end;
        false ->
            check_body_indents(ParentCtx, SpanLines)
    end.


check_continuation_indents(ParentCtx, ContLines) ->
    ParentIndent = emilio_lib:indent_level(emilio_lib:curr_line(ParentCtx)),
    lists:foreach(fun(ContLine) ->
        ContIndent = emilio_lib:indent_level(ContLine),
        if ContIndent >= ParentIndent + 2 -> ok; true ->
            Token = report_token(ContLine),
            ?EMILIO_REPORT(Token, 126)
        end
    end, ContLines).


check_body_indents(ParentCtx, BodyLines) ->
    ParentIndent = emilio_lib:indent_level(emilio_lib:curr_line(ParentCtx)),
    lists:foreach(fun(BodyLine) ->
        case emilio_lib:is_blank_line(BodyLine) of
            true ->
                ok;
            false ->
                BodyIndent = emilio_lib:indent_level(BodyLine),
                if BodyIndent >= ParentIndent + 1 -> ok; true ->
                    Token = report_token(BodyLine),
                    ?EMILIO_REPORT(Token, 125)
                end
        end
    end, BodyLines).


same_line(Parent, SpanStart) ->
    {PL, _} = emilio_anno:lc(Parent),
    {SL, _} = emilio_anno:lc(SpanStart),
    PL == SL.


report_token(Line) ->
    DropFun = fun({white_space, _, _}) -> true; (_) -> false end,
    case lists:dropwhile(DropFun, Line) of
        [NonWhiteSpace | _] ->
            NonWhiteSpace;
        [] ->
            lists:last(Line)
    end.


find_prev_ref(Ctx, Ref) ->
    find_ref(Ctx, Ref, iter_rev).


find_next_ref(Ctx, Ref) ->
    find_ref(Ctx, Ref, iter_fwd).


-define(IGNORE_TOKENS, ['when']).


find_ref(Ctx, Ref, FunName) ->
    IterFun = fun(Token, TokenCtx, Acc) ->
        TokenName = element(1, Token),
        case lists:member(TokenName, ?IGNORE_TOKENS) of
            true ->
                {continue, Acc};
            false ->
                case emilio_anno:ref(Token) of
                    Ref ->
                        {stop, {Token, TokenCtx}};
                    _ ->
                        {continue, Acc}
                end
        end
    end,
    emilio_lib:FunName(Ctx, IterFun, undefined).
