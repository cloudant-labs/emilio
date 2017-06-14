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

-module(emilio_check_ws_lists).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [240, 241, 242, 243].


explain(240) ->
    "There should be no whitespace after a [";
explain(241) ->
    "There should be no whitespace before a ]";
explain(242) ->
    "There should be whitespace before a |";
explain(243) ->
    "There should be whitespace after a |".


format_error(240, _) ->
    "whitespace after [";
format_error(241, _) ->
    "whitespace before ]";
format_error(242, _) ->
    "missing whitespace before |";
format_error(243, _) ->
    "missing whitespace after |".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, Token, Ctx) ->
    case element(1, Token) of
        cons ->
            case ws_after(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 240);
                false -> ok
            end;
        ']' ->
            case ws_before(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 241);
                false -> ok
            end;
        '|' ->
            case emilio_lib:iter_rev(Ctx, fun skip_sep/3, undefined) of
                {white_space, _, _} -> ok;
                _ -> ?EMILIO_REPORT(Anno, 242)
            end,
            case emilio_lib:iter_fwd(Ctx, fun skip_sep/3, undefined) of
                {white_space, _, _} -> ok;
                _ -> ?EMILIO_REPORT(Anno, 243)
            end;
        _ ->
            ok
    end.


ws_before(Ctx) ->
    % Similar to open parens, we have to allow white
    % space before close parens so that it can be on
    % its own line after closing a multi-line
    % argument list.
    case get_prev(Ctx) of
        {white_space, Anno, "\n"} ->
            false;
        {white_space, Anno, _} ->
            case emilio_anno:lc(Anno) of
                {_, 1} -> false;
                _ -> true
            end;
        _ ->
            false
    end.


ws_after(Ctx) ->
    % We allow for newlines so we can do multi-line
    % argument lists for line length
    case get_next(Ctx) of
        {white_space, _, "\n"} -> false;
        {white_space, _, _} -> true;
        _ -> false
    end.


get_prev(Ctx) ->
    emilio_lib:iter_rev(Ctx, fun skip_sep/3, undefined).


get_next(Ctx) ->
    emilio_lib:iter_fwd(Ctx, fun skip_sep/3, undefined).


skip_sep({sep, _}, _Ctx, Acc) ->
    {continue, Acc};
skip_sep(Token, _Ctx, _Acc) ->
    {stop, Token}.
