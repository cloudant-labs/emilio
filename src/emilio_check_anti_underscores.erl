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
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [601].


explain(601) ->
    "Underscore prefixed variables should not be reused".


format_error(601, Name) ->
    io_lib:format("underscore varibale ~s reused", [Name]).


run(Lines) ->
    emilio_lib:fold_tokens(fun check_reuse/3, {[], []}, Lines).


check_reuse(Token, _, {Refs, RefVars}) ->
    %io:format(standard_error, "~p :: ~p :: ~p~n", [Token, Refs, RefVars]),
    {NewRefs, NewRefVars} = case {element(1, Token), Refs, RefVars} of
        {function, [], []} ->
            Ref = emilio_anno:ref(Token),
            {[Ref], [Ref]};
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
        {span_start, _, _} ->
            {Refs, RefVars};
        {span_end, _, _} ->
            {Refs, RefVars};
        {_, _, _} ->
            case emilio_anno:ref(Token) of
                Ref when is_reference(Ref) ->
                    case Refs of
                        [Ref | _] ->
                            RestRefVars = pop_to_ref(Ref, RefVars),
                            {Refs, RestRefVars};
                        [_ | _] ->
                            {[Ref | Refs], [Ref | RefVars]}
                    end;
                undefined ->
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
