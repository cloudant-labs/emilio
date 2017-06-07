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

-export([
    start_job/2
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
        config,
        $c,
        "config",
        'string',
        "The config file to use [default: emilio.cfg]"
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
    case lists:keyfind(help, 1, Opts) of
        {help, true} ->
            usage(0);
        _ ->
            run(Opts, Args)
    end.


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
    process(Args, []).


process([], _Jobs) ->
    {ok, Count} = emilio_report:wait(),
    Status = if
        Count == 0 -> 0;
        true -> 2
    end,
    emilio_util:shutdown(Status);

process([Path | Rest], Jobs) ->
    NewJobs = emilio_path:walk(Path, fun process_file/2, Jobs),
    process(Rest, NewJobs).


process_file(FileName, Jobs) ->
    JobCount = emilio_cfg:get(jobs),
    case length(Jobs) < JobCount of
        true when JobCount == 1 ->
            process_file(FileName),
            Jobs;
        true ->
            [{start_job(FileName), FileName} | Jobs];
        false ->
            NewJobs = wait_for_job(Jobs),
            process_file(FileName, NewJobs)
    end.


wait_for_job(Jobs) ->
    receive
        {'DOWN', Ref, process, _Pid, normal} ->
            lists:keydelete(Ref, 1, Jobs);
        {'DOWN', Ref, process, _Pid, Reason} ->
            {Ref, FileName} = lists:keyfind(Ref, 1, Jobs),
            Args = [FileName, Reason],
            emilio_log:error("Failed to process file: ~s :: ~p~n", Args),
            emilio_util:shutdown(3)
    after 30000 ->
        Files = [FileName || {_, FileName} <- Jobs],
        FileList = string:join(Files, ", "),
        emilio_log:error("Timed out waiting for files: ~s~n", [FileList]),
        emilio_util:shutdown(3)
    end.


start_job(FileName) ->
    {_, Ref} = spawn_monitor(?MODULE, start_job, [self(), FileName]),
    receive
        {started, FileName} ->
            Ref;
        {'DOWN', Ref, process, _Pid, Reason} ->
            Args = [FileName, Reason],
            emilio_log:error("Failed to start job for ~s :: ~p", Args),
            emilio_util:shutdown(3)
    end.


start_job(Pid, FileName) ->
    Pid ! {started, FileName},
    process_file(FileName).


process_file(FileName) ->
    case filename:extension(FileName) of
        ".erl" -> run_checks(FileName);
        ".hrl" -> run_checks(FileName);
        _ -> ok
    end.


run_checks(FileName) ->
    put(emilio_curr_file, FileName),
    emilio_report:queue(FileName),
    Tokens = emilio_pp:file(FileName),
    lists:foreach(fun(Check) ->
        Check:run(Tokens)
    end, ?EMILIO_CHECKS),
    emilio_report:finish(FileName).
