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
    [301].


explain(301) ->
    "There should not be more than two consecutive empty lines".


format_error(301, Count) ->
    io_lib:format("found ~b consecutive empty lines", [Count]).


run(Lines) ->
    Forms = emilio_lib:simplified_forms(Lines),
    lists:foreach(fun(Form) ->
        case Form of
            {new_line, Anno, Count} when Count > 3 ->
                ?EMILIO_REPORT(Anno, 301, Count - 1);
            _ ->
                ok
        end
    end, Forms).
