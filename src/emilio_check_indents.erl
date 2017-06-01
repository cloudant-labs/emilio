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
    [113].


explain(113) ->
    "Indentation should only increase by one or two levels".


format_error(113, Levels) ->
    io_lib:format("indentation increased by ~b levels", [Levels]).


run(Lines) ->
    Units = emilio_cfg:get_int(indentation, units, 4),
    emilio_lib:foreach_line(fun(Loc, Line, Ctx) ->
        check_line(Loc, Line, Ctx, Units)
    end, Lines).


check_line(Loc, Line, Ctx, Units) ->
    CurrIndent = indent_count(Line, Units, []),
    PrevLine = emilio_lib:prev_line(Ctx),
    case PrevLine of
        undefined when CurrIndent > 0 ->
            ?EMILIO_REPORT(Loc, 113, CurrIndent);
        undefined when CurrIndent == 0 ->
            ok;
        _ ->
            PrevIndent = indent_count(PrevLine, Units, []),
            if CurrIndent - PrevIndent =< 2 -> ok; true ->
                ?EMILIO_REPORT(Loc, 113, CurrIndent - PrevIndent)
            end
    end.


indent_count([{white_space, _, "\n"} | _], Units, Acc) ->
    calc_indent(Units, Acc);

indent_count([{white_space, _, Text} | Rest], Units, Acc) ->
    indent_count(Rest, Units, [Text | Acc]);

indent_count(_, Units, Acc) ->
    calc_indent(Units, Acc).


calc_indent(Units, Text) ->
    Spaces = length(lists:flatten(lists:reverse(Text))),
    Base = Spaces div Units,
    Base + if Spaces rem Units == 0 -> 0; true -> 1 end.
