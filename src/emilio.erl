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
    {
        help,
        $h,
        "help",
        'boolean',
        "Show this help message"
    },
    {
        list,
        $l,
        "list",
        'boolean',
        "List all error codes with a short description"
    },
    {
        explain,
        $e,
        "explain",
        'integer',
        "Show explanation for the given error code"
    },
    {
        config,
        $c,
        "config",
        'string',
        "The config file to use"
    },
    {
        ignore,
        $i,
        "ignore",
        'string',
        "Ignore any file path matching the specified glob"
    },
    {
        jobs,
        $j,
        "jobs",
        'integer',
        "Number of files to process in parallel [default: 4]"
    },
    {
        report_formatter,
        $f,
        "format",
        'string',
        "Set the output format [default: text]"
    },
    {
        context,
        $C,
        "context",
        'integer',
        "Number of contextual lines to display for text output [default: 0]"
    },
    {
        whitelist,
        $w,
        "whitelist",
        'string',
        "A CSV file of filename,line,column,code reports to ignore"
    }
]).


main(Argv) ->
    case getopt:parse(?OPTIONS, Argv) of
        {ok, {Opts, Args}} ->
            execute(Opts, Args);
        _ ->
            usage(1)
    end.


execute(Opts, Args) ->
    Handlers = [
        {help, fun(_) -> usage(0) end},
        {list, fun(_) -> emilio_docs:list_codes() end},
        {explain, fun emilio_docs:explain_code/1}
    ],
    lists:foreach(fun({Arg, Handler}) ->
        case lists:keyfind(Arg, 1, Opts) of
            {Arg, Value} ->
                Handler(Value);
            _ ->
                ok
        end
    end, Handlers),
    run(Opts, Args).


usage(Status) ->
    Name = escript:script_name(),
    Extra = "path [path ...]",
    Help = [
        {"path", "Paths to process, directories are searched recursively"}
    ],
    getopt:usage(?OPTIONS, Name, Extra, Help),
    emilio_util:shutdown(Status).


run(Opts, Args) ->
    emilio_cfg:compile(Opts),
    emilio_report:start_link(),
    walk_each(Args).


walk_each(Paths) ->
    Ignores = emilio_cfg:get(ignore),
    lists:foreach(fun(Path) ->
        emilio_path:walk(Path, fun process_file/2, Ignores)
    end, Paths),
    {ok, _FileCount, ErrorCount} = emilio_report:wait(),
    Status = if
        ErrorCount == 0 -> 0;
        true -> 2
    end,
    emilio_util:shutdown(Status).


process_file(FileName, Ignores) ->
    ShouldProcess = case filename:extension(FileName) of
        ".erl" -> true;
        ".hrl" -> true;
        _ -> false
    end,
    ShouldIgnore = lists:foldl(fun(Pattern, Acc) ->
        Acc orelse glob:matches(FileName, Pattern)
    end, false, Ignores),
    case ShouldProcess andalso not ShouldIgnore of
        true ->
            emilio_report:queue(FileName);
        false ->
            ok
    end,
    Ignores.
