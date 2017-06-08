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

-module(emilio_report).


-export([
    start_link/0,
    queue/1,
    update/5,
    wait/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(FORMATTERS, [
    {text, emilio_report_formatter_text},
    {summary, emilio_report_formatter_summary},
    {csv, emilio_report_formatter_csv},
    {json, emilio_report_formatter_json}
]).


-record(st, {
    files = queue:new(),
    started = queue:new(),
    jobs = dict:new(),
    reports = dict:new(),
    finished = sets:new(),
    file_count = 0,
    error_count = 0,
    formatter,
    formatter_st,
    queue_waiter,
    finish_waiter
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


queue(FileName) ->
    gen_server:call(?MODULE, {queue, FileName}, infinity).


update(FileName, Module, Anno, Code, Arg) ->
    {Line, Col} = emilio_anno:lc(Anno),
    ErrMsg = Module:format_error(Code, Arg),
    Msg = {update, FileName, Line, Col, Code, ErrMsg},
    gen_server:call(?MODULE, Msg, infinity).


wait() ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    Resp = gen_server:call(?MODULE, wait, infinity),
    receive
        {'DOWN', Ref, process, _, _} ->
            Resp
    after 1000 ->
        Resp
    end.


init(_) ->
    Fmt = get_formatter(),
    {ok, FmtSt} = Fmt:init(),
    {ok, #st{
        formatter = Fmt,
        formatter_st = FmtSt
    }}.


terminate(_Reason, St) ->
    #st{
        file_count = FCount,
        error_count = ECount,
        formatter = Fmt,
        formatter_st = FmtSt
    } = St,
    Fmt:terminate(FCount, ECount, FmtSt).


handle_call({queue, FileName}, From, #st{queue_waiter = undefined} = St0) ->
    #st{
        files = Files,
        file_count = FCount
    } = St0,
    NewFiles = queue:in(FileName, Files),
    St1 = St0#st{
        files = NewFiles,
        file_count = FCount + 1
    },
    case maybe_start_job(St1) of
        {ok, St2} ->
            {reply, ok, St2};
        full ->
            St2 = St1#st{
                queue_waiter = From
            },
            {noreply, St2}
    end;

handle_call({queue, _}, _From, St) ->
    {reply, {error, queue_waiter_exists}, St};

handle_call({update, FileName, Line, Col, Code, Msg}, _From, St) ->
    #st{
        reports = Reports,
        error_count = ECount
    } = St,
    NewReports = case dict:is_key(FileName, Reports) of
        true ->
            dict:append(FileName, {Line, Col, Code, Msg}, Reports);
        false ->
            dict:store(FileName, [{Line, Col, Code, Msg}], Reports)
    end,
    NewSt = St#st{
        reports = NewReports,
        error_count = ECount + 1
    },
    {reply, ok, NewSt};

handle_call(wait, From, #st{finish_waiter = undefined} = St) ->
    #st{
        started = Started,
        file_count = FCount,
        error_count = ECount
    } = St,
    case queue:is_empty(Started) of
        true ->
            gen_server:reply(From, {ok, FCount, ECount}),
            {stop, normal, St};
        false ->
            {noreply, St#st{finish_waiter = From}}
    end;

handle_call(wait, _From, St) ->
    {reply, {error, finish_waiter_exists}, St};

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', Ref, process, _, Reason}, St0) ->
    #st{
        jobs = Jobs,
        finished = Finished,
        queue_waiter = Waiter
    } = St0,
    FileName = dict:fetch(Ref, Jobs),
    St1 = if Reason == normal -> St0; true ->
        Fmt = "an error occurred while processing ~s :: ~p",
        ErrMsg = io_lib:format(Fmt, [FileName, Reason]),
        Msg = {update, FileName, 0, 0, 903, ErrMsg},
        {reply, ok, S} = handle_call(Msg, nil, St0),
        S
    end,
    if Waiter == undefined -> ok; true ->
        gen_server:reply(Waiter, ok)
    end,
    NewJobs = dict:erase(Ref, Jobs),
    NewFinished = sets:add_element(FileName, Finished),
    St2 = St1#st{
        jobs = NewJobs,
        finished = NewFinished,
        queue_waiter = undefined
    },
    drain(start_jobs(St2));

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_Vsn, St, _Extra) ->
    {ok, St}.


maybe_start_job(St) ->
    #st{
        files = Files,
        started = Started,
        jobs = Jobs
    } = St,
    case queue:peek(Files) of
        empty ->
            {ok, St};
        {value, FileName} ->
            case dict:size(Jobs) < emilio_cfg:get(jobs) of
                true ->
                    {{value, FileName}, NewFiles} = queue:out(Files),
                    NewStarted = queue:in(FileName, Started),
                    {_, Ref} = emilio_job:start(FileName),
                    NewJobs = dict:store(Ref, FileName, Jobs),
                    NewSt = St#st{
                        files = NewFiles,
                        started = NewStarted,
                        jobs = NewJobs
                    },
                    {ok, NewSt};
                false ->
                    full
            end
    end.


start_jobs(St) ->
    case queue:peek(St#st.files) of
        empty ->
            St;
        _ ->
            case maybe_start_job(St) of
                {ok, NewSt} ->
                    start_jobs(NewSt);
                full ->
                    St
            end
    end.


drain(St) ->
    #st{
        started = Started,
        finished = Finished
    } = St,
    case queue:peek(Started) of
        empty ->
            {noreply, St};
        {value, FileName} ->
            case sets:is_element(FileName, Finished) of
                true ->
                    format_report(St, FileName);
                false ->
                    {noreply, St}
            end
    end.


format_report(St, FileName) ->
    #st{
        started = Started,
        reports = Reports,
        finished = Finished,
        file_count = FCount,
        error_count = ECount,
        formatter = Fmt,
        formatter_st = FmtSt,
        finish_waiter = Waiter
    } = St,
    FileReports = case dict:find(FileName, Reports) of
        {ok, R} -> lists:sort(R);
        error -> []
    end,
    NewFmtSt = lists:foldl(fun({Line, Col, Code, Msg}, Acc) ->
        Fmt:format(FileName, Line, Col, Code, Msg, Acc)
    end, FmtSt, FileReports),
    {{value, FileName}, NewStarted} = queue:out(Started),
    NewReports = dict:erase(FileName, Reports),
    NewFinished = sets:del_element(FileName, Finished),
    NewSt = St#st{
        started = NewStarted,
        reports = NewReports,
        finished = NewFinished,
        formatter_st = NewFmtSt
    },
    case queue:is_empty(NewStarted) of
        true when Waiter /= undefined ->
            gen_server:reply(Waiter, {ok, FCount, ECount}),
            {stop, normal, NewSt};
        _ ->
            drain(NewSt)
    end.


get_formatter() ->
    Fmt = emilio_cfg:get(report_formatter),
    {_, Formatter} = if
        is_atom(Fmt) -> lists:keyfind(Fmt, 1, ?FORMATTERS);
        is_list(Fmt) -> lists:keyfind(list_to_atom(Fmt), 1, ?FORMATTERS)
    end,
    Formatter.
