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

-module(emilio).


-export([
    main/1
]).


-include("emilio.hrl").


main([FilePath]) ->
    emilio_path:walk(FilePath, fun process_file/1);

main(_) ->
    io:format(standard_error, "usage: ~s FILE~n", [escript:script_name()]).


process_file(FileName) ->
    case filename:extension(FileName) of
        ".erl" -> run_checks(FileName);
        ".hrl" -> run_checks(FileName);
        _ -> ok
    end.


run_checks(FileName) ->
    io:format(standard_error, "~s~n", [FileName]),
    Tokens = emilio_pp:file(FileName),
    lists:foreach(fun(Check) ->
        Check:run(Tokens)
    end, ?EMILIO_CHECKS),
    io:format(standard_error, "~n", []).
