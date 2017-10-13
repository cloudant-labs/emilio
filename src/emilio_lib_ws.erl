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

-module(emilio_lib_ws).

-export([
    ws_before/1,
    ws_after/1,

    skip_span/3
]).


ws_before(Ctx) ->
    % Similar to open parens, we have to allow white
    % space before close parens so that it can be on
    % its own line after closing a multi-line
    % argument list.
    case get_prev(Ctx) of
        {white_space, _, "\n"} ->
            % We happened to be the first token on the line
            false;
        {white_space, Anno, _} ->
            case emilio_anno:lc(Anno) of
                {_, 1} -> false;
                _ -> true
            end;
        _ ->
            false
    end.


ws_after(Ctx) ->
    % We allow for newlines so we can do multi-line
    % argument lists for line length
    case get_next(Ctx) of
        {white_space, _, "\n"} -> false;
        {white_space, _, _} -> true;
        _ -> false
    end.


skip_span({span_start, _, _}, _Ctx, Acc) ->
    {continue, Acc};
skip_span({span_end, _, _}, _Ctx, Acc) ->
    {continue, Acc};
skip_span(Token, _Ctx, _Acc) ->
    {stop, Token}.


get_prev(Ctx) ->
    emilio_lib:iter_rev(Ctx, fun skip_span/3, undefined).


get_next(Ctx) ->
    emilio_lib:iter_fwd(Ctx, fun skip_span/3, undefined).
