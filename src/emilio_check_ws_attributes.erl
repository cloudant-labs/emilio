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

-module(emilio_check_ws_attributes).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).



-include("emilio.hrl").


codes() ->
    [225].


explain(225) ->
    "attributes should start in the first column of the line".


format_error(225, _) ->
    "attribute definition does not start at the first column of the line".


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, Token, _Ctx) when element(1, Token) == attribute ->
    Anno = element(2, Token),
    case emilio_anno:lc(Anno) of
        {_, 2} -> % 2 because the hyphen is not included
            ok;
        _ ->
            ?EMILIO_REPORT(Anno, 225)
    end;

check(_, _, _) ->
    ok.
