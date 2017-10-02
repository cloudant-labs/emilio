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

-module(emilio_check_exports_order).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [420, 421].


explain(420) ->
    "Exports and functions should follow the same order";
explain(421) ->
    "Private functions should come after exported functions".


format_error(420, {{Export, EArity}, {Function, FArity}}) ->
    Fmt = "expected ~s/~b, but found ~s/~b",
    io_lib:format(Fmt, [Export, EArity, Function, FArity]);
format_error(421, {Function, FArity}) ->
    Fmt = "private function ~s/~b found in exported functions",
    io_lib:format(Fmt, [Function, FArity]).


run(Lines) ->
    Exports = find_exports(Lines),
    Functions = find_functions(Lines),
    check_order(Exports, Functions, undefined, Exports).


check_order([], _, _, _) ->
    ok;

check_order(Exports, Functions, PrevFun, AllExports) ->
    [{Export, EArity} | RestExports] = Exports,
    [Function | RestFunctions] = Functions,
    FName = element(3, Function),
    FArity = element(4, Function),
    IsExported = lists:member({FName, FArity}, AllExports),
    if
        FName == Export andalso FArity == EArity ->
            % Found an expected function, carry on
            check_order(RestExports, RestFunctions, Export, AllExports);
        FName == PrevFun andalso not IsExported ->
            check_order(Exports, RestFunctions, PrevFun, AllExports);
        IsExported ->
            ?EMILIO_REPORT(Function, 420, {{Export, EArity}, {FName, FArity}}),
            check_order(Exports, RestFunctions, PrevFun, AllExports);
        true ->
            ?EMILIO_REPORT(Function, 421, {FName, FArity}),
            check_order(Exports, RestFunctions, PrevFun, AllExports)
    end.


find_exports(Lines) ->
    Exports = emilio_lib:fold_tokens(fun(Token, _, Acc) ->
        case element(1, Token) of
            export ->
                Export = {element(3, Token), element(4, Token)},
                {continue, [Export | Acc]};
            _ ->
                {continue, Acc}
        end
    end, [], Lines),
    lists:reverse(Exports).


find_functions(Lines) ->
    Functions = emilio_lib:fold_tokens(fun(Token, _, Acc) ->
        case element(1, Token) of
            function ->
                {continue, [Token | Acc]};
            _ ->
                {continue, Acc}
        end
    end, [], Lines),
    lists:reverse(Functions).
