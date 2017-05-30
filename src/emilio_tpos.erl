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

-module(emilio_tpos).


-export([
    new/0,
    get_name/1,
    get_depth/1,

    enter/2,
    leave/2
]).


-record(tpos, {
    pos = []
}).


new() ->
    #tpos{}.


get_name(#tpos{pos = []}) ->
    root;

get_name(#tpos{pos = [Pos | _]}) ->
    Pos.


get_depth(#tpos{}) ->
    0.


enter(Token, TPos) ->
    push(Token, TPos).


leave({dot, _}, TPos) ->
    TPos#tpos{pos = []};

leave({white_space, _, _}, TPos) ->
    pop(TPos);

leave({')', _}, TPos) ->
    io:format(standard_error, "TPOS: ~p~n", [TPos]),
    pop_paren(TPos);

leave(_Token, TPos) ->
    TPos.


push(Term, #tpos{pos = Pos} = T) ->
    T#tpos{pos = [Term | Pos]}.


pop(#tpos{pos = [_ | Pos]} = T) ->
    T#tpos{pos = Pos}.


pop_paren(#tpos{pos = [Elem | Rest]} = TPos) ->
    NewTPos = TPos#tpos{pos = Rest},
    case element(1, Elem) of
        '(' -> NewTPos;
        fun_clause -> NewTPos;
        _ -> pop_paren(NewTPos)
    end.
