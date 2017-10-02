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
    emilio_lib:foreach_token(fun check_exports/3, Lines).


check_exports(Anno, Token, Ctx) ->
    case element(1, Token) of
        export ->
            CurrLine = emilio_lib:curr_line(Ctx),
            case emilio_lib:indent_level(CurrLine) of
                1 ->
                    ok;
                N ->
                    ?EMILIO_REPORT(Anno, 130, N)
            end;
        _ ->
            ok
    end.
