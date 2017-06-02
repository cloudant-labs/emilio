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

-module(emilio_check_indents_match).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [113].


explain(113) ->
    "Indentation for various tokens should match when split across lines".


format_error(113, {Start, StartIndent, End, EndIndent}) ->
    StartName = element(1, Start),
    EndName = element(1, End),
    Fmt = "mismatched indentation level ~b vs ~b for ~s/~s tokens",
    io_lib:format(Fmt, [StartIndent, EndIndent, StartName, EndName]).


run(Lines) ->
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(Anno, {'after', _} = Token, Ctx) ->
    match_indent(Anno, Token, Ctx);

check_token(Anno, {'catch', _} = Token, Ctx) ->
    case emilio_anno:ref(Token) of
        undefined ->
            % This is a catch expression, not a keyword
            % for a try/catch clause
            ok;
        _ ->
            match_indent(Anno, Token, Ctx)
    end;

check_token(Anno, {'end', _} = Token, Ctx) ->
    match_indent(Anno, Token, Ctx);

check_token(Anno, {'of', _} = Token, Ctx) ->
    match_indent(Anno, Token, Ctx);

check_token(_, _, _) ->
    ok.


match_indent(Anno, Token, Ctx) ->
    {Line, _Col} = emilio_anno:lc(Token),
    Ref = emilio_anno:ref(Token),
    {ok, MatchToken, MatchCtx} = emilio_lib:find_ref_rev(Ctx, Ref),
    case emilio_anno:lc(MatchToken) of
        {Line, _} ->
            % Tokens on same line means nothing to check
            ok;
        {_, _} ->
            TokenLine = emilio_lib:curr_line(Ctx),
            MatchLine = emilio_lib:curr_line(MatchCtx),
            check_indent_matches(Anno, MatchToken, MatchLine, Token, TokenLine)
    end.


check_indent_matches(Anno, MatchToken, MatchLine, Token, TokenLine) ->
    MatchLevel = emilio_lib:indent_level(MatchLine),
    TokenLevel = emilio_lib:indent_level(TokenLine),
    if MatchLevel == TokenLevel -> ok; true ->
        ?EMILIO_REPORT(Anno, 113, {MatchToken, MatchLevel, Token, TokenLevel})
    end.
