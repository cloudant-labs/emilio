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

-module(emilio_check_ws_space_after_comma).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [222].


explain(222) ->
    "Commas should be followed by a single space or a newline".


format_error(222, _) ->
    "comma missing single space or newline".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, {',', _}, Ctx) ->
    case emilio_lib:next_token(Ctx) of
        {'white_space', _, " "} -> ok;
        {'white_space', _, "\n"} -> ok;
        _ -> ?EMILIO_REPORT(Anno, 222)
    end;

check(_, _, _) ->
    ok.
