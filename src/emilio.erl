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


-define(OPTIONS, [
    {help, $h, "help", 'boolean', "Show this help message"}
]).


main(Argv) ->
    case getopt:parse(?OPTIONS, Argv) of
        {ok, {Opts, Args}} ->
            run(Opts, Args);
        _ ->
            usage(1)
    end.


run(Opts, Args) ->
    case lists:keyfind(help, 1, Opts) of
        {help, true} ->
            usage(0);
        _ ->
            process(Args)
    end.


usage(Status) ->
    Name = escript:script_name(),
    Extra = "path [path ...]",
    Help = [{"path", "Paths to process, directories searched recursively"}],
    Out = getopt:usage(?OPTIONS, Name, Extra, Help),
    io:format(standard_error, "~s", [Out]),
    init:stop(Status).


process([]) ->
    init:stop(0);

process([Path | Rest]) ->
    emilio_path:walk(Path, fun process_file/1),
    process(Rest).


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
