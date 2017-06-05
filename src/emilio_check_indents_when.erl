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

-module(emilio_check_indents_when).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


-define(CLAUSES, [
    function_clause,
    case_clause,
    try_case_clause,
    try_catch_clause,
    receive_clause,
    fun_clause,
    named_fun_clause
]).


codes() ->
    [122].


explain(122) ->
    "When should be indented two levels so its not confused with "
    "the clause body.".


format_error(122, {Token, Levels}) ->
    Name = element(1, Token),
    Fmt = "indentation for ~s 'when' was ~b, not 2",
    io_lib:format(Fmt, [Name, Levels]).


run(Lines) ->
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(_, {'when', _} = When, Ctx) ->
    check_when(When, Ctx);

check_token(_, _, _) ->
    ok.


check_when({'when', Anno}, Ctx) ->
    {Line, _Col} = emilio_anno:lc(Anno),
    case emilio_anno:ref(Anno) of
        undefined ->
            % 'when' token from a type spec
            ok;
        Ref ->
            {ok, Match, MCtx} = emilio_lib:find_ref_rev(Ctx, Ref, ?CLAUSES),
            case emilio_anno:lc(Match) of
                {Line, _} ->
                    % Tokens on same line means nothing to check
                    ok;
                {_, _} ->
                    TokenLine = emilio_lib:curr_line(Ctx),
                    MatchLine = emilio_lib:curr_line(MCtx),
                    check_indent_increases(Anno, Match, MatchLine, TokenLine)
            end
    end.


check_indent_increases(Anno, MatchToken, MatchLine, TokenLine) ->
    MatchLevel = emilio_lib:indent_level(MatchLine),
    TokenLevel = emilio_lib:indent_level(TokenLine),
    Diff = TokenLevel - MatchLevel,
    if Diff == 2 -> ok; true ->
        ?EMILIO_REPORT(Anno, 122, {MatchToken, Diff})
    end.
