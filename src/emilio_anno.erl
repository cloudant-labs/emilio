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

-module(emilio_anno).


-export([
    lc/1,
    ref/1,

    inc_line/1,
    inc_col/1,
    copy_ref/2
]).


lc(Anno) when is_list(Anno) ->
    {line, Line} = lists:keyfind(line, 1, Anno),
    {column, Col} = lists:keyfind(column, 1, Anno),
    {Line, Col};

lc(Tuple) when is_tuple(Tuple), size(Tuple) >= 2 ->
    lc(element(2, Tuple)).


ref(Anno) when is_list(Anno) ->
    case lists:keyfind(ref, 1, Anno) of
        {ref, Ref} -> Ref;
        false -> undefined
    end;

ref(Tuple) when is_tuple(Tuple), size(Tuple) >= 2 ->
    ref(element(2, Tuple)).


inc_line(Anno) when is_list(Anno) ->
    {Line, _} = lc(Anno),
    A0 = lists:keystore(line, 1, Anno, {line, Line + 1}),
    lists:keystore(column, 1, A0, {column, 1}).


inc_col(Anno) when is_list(Anno) ->
    {_Line, Col} = lc(Anno),
    lists:keystore(column, 1, Anno, {column, Col + 1}).


copy_ref(SrcAnno, DstAnno) when is_list(SrcAnno), is_list(DstAnno) ->
    Ref = ref(SrcAnno),
    false = lists:keyfind(ref, 1, DstAnno),
    lists:keystore(ref, 1, DstAnno, {ref, Ref});

copy_ref(Anno, Tuple) when is_list(Anno), is_tuple(Tuple), size(Tuple) >= 2 ->
    NewAnno = copy_ref(Anno, element(2, Tuple)),
    setelement(2, Tuple, NewAnno);

copy_ref(Tuple1, Tuple2) when is_tuple(Tuple1), size(Tuple1) >= 2 ->
    copy_ref(element(2, Tuple1), Tuple2).
