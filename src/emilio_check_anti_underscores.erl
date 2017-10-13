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

-module(emilio_check_anti_underscores).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(601, Name) ->
    io_lib:format("underscore varibale ~s reused", [Name]).


run(Lines) ->
    emilio_lib:fold_tokens(fun check_reuse/3, {[], []}, Lines).


check_reuse(Token, _, {Refs, RefVars}) ->
    {NewRefs, NewRefVars} = case {element(1, Token), Refs, RefVars} of
        {function, [], []} ->
            Ref = emilio_anno:ref(Token),
            {[Ref], [Ref]};
        {_, [], []} ->
            {[], []};
        {dot, [_], _} ->
            {[], []};
        {'end', _, _} ->
            Ref = emilio_anno:ref(Token),
            [Ref | RestRefs] = Refs,
            RestRefVars = pop_ref(Ref, RefVars),
            {RestRefs, RestRefVars};
        {var, _, _} ->
            Name = element(3, Token),
            NameList = atom_to_list(Name),
            case hd(NameList) of
                $_ when Name /= '_' ->
                    case lists:member(Name, RefVars) of
                        true ->
                            ?EMILIO_REPORT(Token, 601, Name),
                            {Refs, RefVars};
                        false ->
                            {Refs, [Name | RefVars]}
                    end;
                _ ->
                    {Refs, RefVars}
            end;
        {_, _, _} ->
            case {is_ref_token(Token), emilio_anno:ref(Token)} of
                {true, Ref} when is_reference(Ref) ->
                    case Refs of
                        [Ref | _] ->
                            RestRefVars = pop_to_ref(Ref, RefVars),
                            {Refs, RestRefVars};
                        [_ | _] ->
                            {[Ref | Refs], [Ref | RefVars]}
                    end;
                {false, _} ->
                    {Refs, RefVars}
            end
    end,
    {continue, {NewRefs, NewRefVars}}.


pop_ref(Ref, [Ref | RestRefVars]) ->
    RestRefVars;

pop_ref(Ref, [_ | RestRefVars]) ->
    pop_ref(Ref, RestRefVars).


pop_to_ref(Ref, RefVars) ->
    [Ref | pop_ref(Ref, RefVars)].


is_ref_token({'fun', _, {function, _, _}}) ->
    false;
is_ref_token({'fun', _, {function, _, _, _}}) ->
    false;
is_ref_token(Tuple) when is_tuple(Tuple) ->
    RefTokens = [
        function,
        function_clause,
        block_start,
        'if',
        if_clause,
        'case',
        'of',
        case_clause,
        'try',
        try_case_clause,
        try_catch_clause,
        'after',
        'receive',
        receive_clause,
        'fun',
        fun_clause,
        named_fun,
        named_fun_clause
    ],
    lists:member(element(1, Tuple), RefTokens).
