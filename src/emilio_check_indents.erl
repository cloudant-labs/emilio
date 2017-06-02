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

-module(emilio_check_indents).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [112].


explain(112) ->
    "Indentation should only increase by one or two levels".


format_error(112, Levels) ->
    io_lib:format("indentation increased by ~b levels", [Levels]).


run(Lines) ->
    emilio_lib:foreach_line(fun check_line/3, Lines).


check_line(Anno, Line, Ctx) ->
    CurrIndent = emilio_lib:indent_level(Line),
    PrevLine = get_prev_line(Ctx),
    case PrevLine of
        undefined when CurrIndent > 0 ->
            ?EMILIO_REPORT(Anno, 112, CurrIndent);
        undefined when CurrIndent == 0 ->
            ok;
        _ ->
            PrevIndent = emilio_lib:indent_level(PrevLine),
            if CurrIndent - PrevIndent =< 2 -> ok; true ->
                ?EMILIO_REPORT(Anno, 112, CurrIndent - PrevIndent)
            end
    end.

get_prev_line(Ctx) ->
    case emilio_lib:prev_line(Ctx) of
        {ok, PrevLine, PrevCtx} ->
            case emilio_lib:is_blank_line(PrevLine) of
                true ->
                    get_prev_line(PrevCtx);
                false ->
                    PrevLine
            end;
        undefined ->
            undefined
    end.
