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

-module(emilio_check_mod_empty_lines).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [301, 302].


explain(301) ->
    "There should not be more than two consecutive empty lines";
explain(302) ->
    "There should be two empty lines betwen functions".


format_error(301, Count) ->
    io_lib:format("found ~b consecutive empty lines", [Count]);
format_error(302, Count) when Count < 2 ->
    io_lib:format("missing ~b empty lines between functions", [2 - Count]);
format_error(303, Count) ->
    Fmt = "found an extra ~b empty lines between functions",
    io_lib:format(Fmt, [Count - 2]).


run(Lines) ->
    Forms = emilio_lib:simplified_forms(Lines),
    check_max_empty_lines(Forms),
    check_function_empty_lines(Forms).


check_max_empty_lines(Forms) ->
    lists:foreach(fun(Form) ->
        case Form of
            {new_line, Anno, Count} when Count > 3 ->
                ?EMILIO_REPORT(Anno, 301, Count - 1);
            _ ->
                ok
        end
    end, Forms).


check_function_empty_lines([]) ->
    [];

check_function_empty_lines([{function, _, _, _, _} | Rest]) ->
    case Rest of
        [] ->
            ok;
        [{new_line, _, _}] ->
            ok;
        [{new_line, _, Count} | _] when Count == 3 ->
            ok;
        [{new_line, Anno, Count} | _] ->
            ?EMILIO_REPORT(Anno, 302, Count - 1);
        [Form | _] ->
            ?EMILIO_REPORT(element(2, Form), 302, 0)
    end,
    check_function_empty_lines(Rest);

check_function_empty_lines([_Form | Rest]) ->
    check_function_empty_lines(Rest).
