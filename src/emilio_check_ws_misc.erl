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

-module(emilio_check_ws_misc).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(223, _) ->
    "semicolon preceded by whitespace";
format_error(224, _) ->
    "dot preceded by whitespace".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, Token, Ctx) ->
    FoldFun = fun
        ({white_space, _, _}, _, Code) ->
            ?EMILIO_REPORT(Anno, Code),
            {stop, ok};
        ({span_start, _, _}, _, Code) ->
            {continue, Code};
        ({span_end, _, _}, _, Code) ->
            {continue, Code};
        (_, _, _) ->
            {stop, ok}
    end,
    case Token of
        {';', _} ->
            emilio_lib:iter_rev(Ctx, FoldFun, 223);
        {dot, _} ->
            emilio_lib:iter_rev(Ctx, FoldFun, 224);
        _ ->
            ok
    end.
