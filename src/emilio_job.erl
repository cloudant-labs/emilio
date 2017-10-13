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

-module(emilio_job).


-export([
    start/1
]).

-export([
    init/1
]).


-include("emilio.hrl").


start(FileName) ->
    erlang:spawn_monitor(?MODULE, init, [FileName]).


init(FileName) ->
    try
        run(FileName)
    catch T:R ->
        S = erlang:get_stacktrace(),
        erlang:exit({job_failed, T, R, S})
    end.


run(FileName) ->
    put(emilio_curr_file, FileName),
    emilio_util:time_it(file, FileName, fun() ->
        {ok, Contents} = read_file(FileName),
        Lines = binary:split(Contents, <<"\n">>, [global]),
        emilio_report:cache_file(FileName, Lines),
        Tokens = emilio_util:time_it(op, preprocess, fun() ->
            emilio_pp:file(FileName, Contents)
        end),
        lists:foreach(fun(Check) ->
            emilio_util:time_it(op, Check, fun() ->
                Check:run(Tokens)
            end)
        end, ?EMILIO_CHECKS)
    end).


read_file(FileName) ->
    emilio_util:time_it(op, read_file, fun() ->
        file:read_file(FileName)
    end).
