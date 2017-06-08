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

-module(emilio_report_formatter_summary).


-export([
    init/0,
    terminate/3,
    format/6
]).


init() ->
    {ok, dict:new()}.


terminate(FileCount, ErrorCount, St) ->
    io:format("Found ~b errors in ~b files~n", [ErrorCount, FileCount]),
    io:format(" Code :  Count~n", []),
    lists:foreach(fun({Code, Count}) ->
        io:format("  ~3b : ~6b~n", [Code, Count])
    end, lists:sort(dict:to_list(St))).


format(_FileName, _Line, _Col, Code, _Msg, St) ->
    dict:update_counter(Code, 1, St).
