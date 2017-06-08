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

-module(emilio_report_formatter_json).


-export([
    init/0,
    terminate/3,
    format/6
]).


-record(st, {
    curr_file,
    reports = []
}).


init() ->
    {ok, #st{}}.


terminate(_FileCount, _ErrorCount, St) ->
    dump_file(St).


format(FileName, Line, Col, Code, Msg, St) ->
    case FileName == St#st.curr_file orelse St#st.curr_file == undefined of
        true ->
            St#st{
                curr_file = FileName,
                reports = [{Line, Col, Code, Msg} | St#st.reports]
            };
        false ->
            dump_file(St),
            #st{
                curr_file = FileName,
                reports = [{Line, Col, Code, Msg}]
            }
    end.


dump_file(#st{curr_file = FileName, reports = Reports}) ->
    EJson = {struct, [
        {filename, iolist_to_binary(FileName)},
        {reports, json_reports(Reports)}
    ]},
    io:format("~s~n", [mochijson2:encode(EJson)]).


json_reports([]) ->
    [];

json_reports([{Line, Col, Code, Msg} | Rest]) ->
    [{struct, [
        {line, Line},
        {column, Col},
        {code, Code},
        {message, iolist_to_binary(Msg)}
    ]} | json_reports(Rest)].
