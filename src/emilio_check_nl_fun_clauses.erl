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

-module(emilio_check_nl_fun_clauses).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [310, 311].


explain(310) ->
    "There should be zero or one empty lines between function clauses";
explain(311) ->
    "There should be a consistent number of"
        " empty lines between function clauses".


format_error(310, Count) ->
    Fmt = "found an extra ~b empty lines between function clauses",
    io_lib:format(Fmt, [Count - 1]);
format_error(311, _) ->
    "inconsistent empty lines between function clauses".


run(Lines) ->
    Forms = emilio_lib:simplified_forms(Lines),
    check_function_empty_lines(Forms).


check_function_empty_lines([]) ->
    [];

check_function_empty_lines([{function, Anno, _, _, Clauses} | Rest]) ->
    Counts = count_empty_lines(Clauses),
    case lists:usort(Counts) of
        [] -> ok;
        [_] -> ok;
        [_ | _] -> ?EMILIO_REPORT(Anno, 311)
    end,
    check_function_empty_lines(Rest);

check_function_empty_lines([_Form | Rest]) ->
    check_function_empty_lines(Rest).


count_empty_lines([_Clause]) ->
    [];

count_empty_lines([Clause | RestClauses]) ->
    {function_clause, InitAnno, Tokens} = Clause,
    {Anno, Total} = lists:foldl(fun
        ({new_line, NewAnno, Count}, {_, AccTotal}) ->
            {NewAnno, AccTotal + Count - 1};
        (_, Acc) ->
            Acc
    end, {InitAnno, 0}, Tokens),
    if Total =< 1 -> ok; true ->
        ?EMILIO_REPORT(Anno, 310, Total)
    end,
    [Total | count_empty_lines(RestClauses)].
