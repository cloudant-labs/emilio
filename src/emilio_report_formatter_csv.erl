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

-module(emilio_report_formatter_csv).


-export([
    init/0,
    terminate/1,
    format/6
]).


init() ->
    io:format("File,Line,Column,Code,Message~n", []),
    {ok, nil}.


terminate(_St) ->
    ok.


format(FileName, Line, Col, Code, Msg, _St) ->
    io:format("~s,~b,~b,~b,\"~s\"~n", [FileName, Line, Col, Code, Msg]).
