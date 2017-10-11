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

-module(emilio_check_ws_file_newline).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(210, Count) ->
    io_lib:format("file ends with ~b new lines, not 1", [Count]).


run([]) ->
    ok;

run(Lines) ->
    RevTokens = lists:reverse(lists:flatten(Lines)),
    Anno = element(2, hd(RevTokens)),
    Count = count_newlines(RevTokens, 0),
    if Count == 1 -> ok; true ->
        ?EMILIO_REPORT(Anno, 210, Count)
    end.


count_newlines([{white_space, _, "\n"} | Rest], Count) ->
    count_newlines(Rest, Count + 1);
count_newlines(_, Count) ->
    Count.
