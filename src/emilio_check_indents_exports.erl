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

-module(emilio_check_indents_exports).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [130].


explain(130) ->
    "export attribute ".


format_error(130, Levels) ->
    io_lib:format("export indented ~b levels, not 1", [Levels]).


run(Lines) ->
    case find_export_group(Lines) of
        {Export, RestLines} ->
            check_export(Export),
            run(RestLines);
        not_found ->
            ok
    end.


check_export([_]) ->
    % Single line attribute so no indentation
    ok;

check_export([_FirstLine | RestLines]) ->
    {BodyLines, [_LastLine]} = lists:split(length(RestLines) - 1, RestLines),
    lists:foreach(fun(Line) ->
        case emilio_lib:is_blank_line(Line) of
            true ->
                ok;
            false ->
                case emilio_lib:indent_level(Line) of
                    1 ->
                        ok;
                    N ->
                        Anno = element(2, hd(Line)),
                        ?EMILIO_REPORT(Anno, 130, N)
                end
        end
    end, BodyLines).


find_export_group([]) ->
    not_found;

find_export_group([Line | RestLines]) ->
    case drop_before_export(Line) of
        [] ->
            find_export_group(RestLines);
        Tokens ->
            take_until_dot([Tokens | RestLines])
    end.


take_until_dot([Line | Rest]) ->
    case has_dot(Line) of
        true ->
            {WithDot, RestLine} = split_after_dot(Line),
            {WithDot, [RestLine | Rest]};
        false ->
            {WithDot, RestLines} = take_until_dot(Rest),
            {[Line | WithDot], RestLines}
    end.


drop_before_export(Line) ->
    Pred = fun(Token) ->
        case Token of
            {attribute, _, export} -> false;
            _ -> true
        end
    end,
    lists:dropwhile(Pred, Line).


has_dot(Line) ->
    lists:foldl(fun(Token, HasDot) ->
        element(1, Token) == dot orelse HasDot
    end, false, Line).


split_after_dot(Line) ->
    Pred = fun(Token) -> element(1, Token) /= dot end,
    {Before, [Dot | After]} = lists:splitwith(Pred, Line),
    {[Before ++ [Dot]], After}.
