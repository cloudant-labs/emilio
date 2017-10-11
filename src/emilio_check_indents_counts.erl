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

-module(emilio_check_indents_counts).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(111, Count) ->
    io_lib:format("leading white space should be a multiple of ~b", [Count]).


run(Lines) ->
    Count = emilio_cfg:get(indentation_count),
    emilio_lib:foreach_line(fun(Loc, Line) ->
        check_line(Loc, Count, Line)
    end, Lines).


check_line(_Loc, _Count, [{white_space, _, [$\n]}]) ->
    ok;

check_line(Loc, Count, [{white_space, _, Text} | _]) ->
    if length(Text) rem Count == 0 -> ok; true ->
        ?EMILIO_REPORT(Loc, 111, Count)
    end;

check_line(_Loc, _Count, _Line) ->
    ok.
