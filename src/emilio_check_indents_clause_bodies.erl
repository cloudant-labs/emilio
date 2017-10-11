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

-module(emilio_check_indents_clause_bodies).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(127, _) ->
    "clause bodies should be all inline or all on new lines, not mixed".


run(Lines) ->
    emilio_lib:foreach_token(fun check_token/3, Lines).


check_token(_, Token, Ctx) ->
    ClauseTypes = clause_types(element(1, Token)),
    lists:foreach(fun(ClauseType) ->
        check_clauses(Ctx, Token, ClauseType)
    end, ClauseTypes).


check_clauses(Ctx, Token, ClauseType) ->
    AccIn = {ClauseType, emilio_anno:ref(Token), []},
    {_, _, RevCList} = emilio_lib:iter_fwd(Ctx, fun collect_clauses/3, AccIn),
    Clauses = lists:reverse(RevCList),
    case Clauses of
        [{_, false}, {_, true}] when ClauseType == if_clause ->
            ok;
        [] ->
            ok;
        [{_, MultiLine} | RestClauses] ->
            check_same(RestClauses, MultiLine)
    end.


check_same([], _) ->
    ok;

check_same([{_, MultiLine} | Rest], MultiLine) ->
    check_same(Rest, MultiLine);

check_same([{Token, _} | _], _) ->
    ?EMILIO_REPORT(Token, 127).


collect_clauses({dot, _}, _Ctx, Acc) ->
    {stop, Acc};

collect_clauses({'end', Anno}, _Ctx, {_, Ref, _} = Acc) ->
    case emilio_anno:ref(Anno) == Ref of
        true ->
            {stop, Acc};
        false ->
            {continue, Acc}
    end;

collect_clauses(Token, Ctx, {ClauseType, Ref, ClauseAcc} = Acc) ->
    TokenName = element(1, Token),
    TokenRef = emilio_anno:ref(Token),
    case TokenName == ClauseType andalso TokenRef == Ref of
        true ->
            NewClause = collect_clause(Token, Ctx, Ref),
            {continue, {ClauseType, Ref, [NewClause | ClauseAcc]}};
        false ->
            {continue, Acc}
    end.


collect_clause(Token, Ctx, Ref) ->
    SpanStart = find_span_start(Ctx, Ref),
    SpanEnd = find_span_end(Ctx, emilio_anno:ref(SpanStart)),
    {TL, _} = emilio_anno:lc(Token),
    {SL, _} = emilio_anno:lc(SpanEnd),
    {Token, SL > TL}.


find_span_start(Ctx, Ref) ->
    IterFun = fun(Token, _, _) ->
        case element(1, Token) of
            span_start ->
                case emilio_anno:parent_ref(Token) == Ref of
                    true ->
                        {stop, Token};
                    false ->
                        {continue, not_found}
                end;
            _ ->
                {continue, not_found}
        end
    end,
    emilio_lib:iter_fwd(Ctx, IterFun, not_found).


find_span_end(Ctx, Ref) ->
    IterFun = fun(Token, _, _) ->
        case element(1, Token) of
            span_end ->
                case emilio_anno:ref(Token) == Ref of
                    true ->
                        {stop, Token};
                    false ->
                        {continue, not_found}
                end;
            _ ->
                {continue, not_found}
        end
    end,
    emilio_lib:iter_fwd(Ctx, IterFun, not_found).


clause_types('function') -> [function_clause];
clause_types('if') -> [if_clause];
clause_types('case') -> [case_clause];
clause_types('try') -> [try_case_clause, try_catch_clause];
clause_types('receive') -> [receive_clause];
clause_types('fun') -> [fun_clause];
clause_types('named_fun') -> [named_fun_clause];
clause_types(_) -> [].
