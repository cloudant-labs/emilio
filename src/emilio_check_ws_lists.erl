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

-module(emilio_check_ws_lists).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(240, _) ->
    "whitespace after [";
format_error(241, _) ->
    "whitespace before ]";
format_error(242, _) ->
    "missing whitespace before |";
format_error(243, _) ->
    "missing whitespace after |".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, Token, Ctx) ->
    case element(1, Token) of
        T when T == cons orelse T == '[' ->
            case emilio_lib_ws:ws_after(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 240);
                false -> ok
            end;
        T when T == nil orelse T == ']' ->
            case emilio_lib_ws:ws_before(Ctx) of
                true -> ?EMILIO_REPORT(Anno, 241);
                false -> ok
            end;
        '|' ->
            Fun = fun emilio_lib_ws:skip_span/3,
            case emilio_lib:iter_rev(Ctx, Fun, undefined) of
                {white_space, _, _} -> ok;
                _ -> ?EMILIO_REPORT(Anno, 242)
            end,
            case emilio_lib:iter_fwd(Ctx, Fun, undefined) of
                {white_space, _, _} -> ok;
                _ -> ?EMILIO_REPORT(Anno, 243)
            end;
        _ ->
            ok
    end.
