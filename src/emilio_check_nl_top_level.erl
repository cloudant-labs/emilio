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

-module(emilio_check_nl_top_level).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(301, _) ->
    "found more than two consecutive empty lines";
format_error(302, Count) ->
    Fmt = "there should be 2 empty lines between functions, not ~b",
    io_lib:format(Fmt, [Count]).


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
