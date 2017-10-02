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


format_error(130, {Indent, Column}) ->
    io_lib:format("export at column ~b, not ~b", [Column, Indent + 1]).


run(Lines) ->
    emilio_lib:foreach_token(fun check_exports/2, Lines).


check_exports(Anno, Token) ->
    case element(1, Token) of
        export ->
            Indent = emilio_cfg:get(indentation_count),
            {_, Col} = emilio_anno:lc(Anno),
            if (Col - 1) == Indent -> ok; true ->
                ?EMILIO_REPORT(Anno, 130, {Indent, Col})
            end;
        _ ->
            ok
    end.
