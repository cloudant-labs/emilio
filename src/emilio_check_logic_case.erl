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

-module(emilio_check_logic_case).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(701, _) ->
    "case statement with a single clause".


run(Lines) ->
    emilio_lib:foreach_token(fun check_case_clauses/2, Lines).


check_case_clauses(_, {'case', Anno, 1}) ->
    ?EMILIO_REPORT(Anno, 701);
check_case_clauses(_, _) ->
    ok.
