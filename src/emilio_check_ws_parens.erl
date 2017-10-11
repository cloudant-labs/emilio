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

-module(emilio_check_ws_parens).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(230, _) ->
    "whitespace after (";
format_error(231, _) ->
    "whitespace before )".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, Token, Ctx) ->
    case element(1, Token) of
        '(' ->
            case emilio_lib_ws:ws_after(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 230);
                false -> ok
            end;
        ')' ->
            case emilio_lib_ws:ws_before(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 231);
                false -> ok
            end;
        _ ->
            ok
    end.
