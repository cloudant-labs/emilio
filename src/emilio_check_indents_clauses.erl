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

-module(emilio_check_indents_clauses).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [121].


explain(121) ->
    "Indentation for clauses should increase by one level".


format_error(121, {Token, Levels}) ->
    Name = element(1, Token),
    Fmt = "indentation change for ~s clause was ~b, not 1",
    io_lib:format(Fmt, [Name, Levels]).


run(Lines) ->
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(Anno, Token, Ctx) ->
    case parent_token(element(1, Token)) of
        undefined ->
            ok;
        Name ->
            check_clause(Anno, Token, Ctx, Name)
    end.


check_clause(Anno, Token, Ctx, ParentName) ->
    {Line, _Col} = emilio_anno:lc(Token),
    Ref = emilio_anno:ref(Token),
    {ok, MatchToken, MatchCtx} = emilio_lib:find_ref_rev(Ctx, Ref, ParentName),
    case emilio_anno:lc(MatchToken) of
        {Line, _} ->
            % Tokens on same line means nothing to check
            ok;
        {_, _} ->
            TokenLine = emilio_lib:curr_line(Ctx),
            MatchLine = emilio_lib:curr_line(MatchCtx),
            check_indent_increases(Anno, MatchToken, MatchLine, TokenLine)
    end.


check_indent_increases(Anno, MatchToken, MatchLine, TokenLine) ->
    MatchLevel = emilio_lib:indent_level(MatchLine),
    TokenLevel = emilio_lib:indent_level(TokenLine),
    Diff = TokenLevel - MatchLevel,
    if Diff == 1 -> ok; true ->
        ?EMILIO_REPORT(Anno, 121, {MatchToken, Diff})
    end.


parent_token(case_clause) -> 'case';
parent_token(try_case_clause) -> 'try';
parent_token(try_catch_clause) -> 'catch';
parent_token(receive_clause) -> 'receive';
parent_token(fun_clause) -> 'fun';
parent_token(named_fun_clause) -> 'named_fun';
parent_token(_) -> undefined.
