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
    store/5,
    finish/1,
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
    reports = dict:new(),
    finished = sets:new(),
    count = 0,
    formatter,
    formatter_st,
    waiter
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


queue(FileName) ->
    gen_server:call(?MODULE, {queue, FileName}).


store(FileName, Module, Anno, Code, Arg) ->
    {Line, Col} = emilio_anno:lc(Anno),
    Msg = Module:format_error(Code, Arg),
    gen_server:call(?MODULE, {store, FileName, Line, Col, Code, Msg}).


finish(FileName) ->
    gen_server:call(?MODULE, {finish, FileName}).


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
        formatter = Fmt,
        formatter_st = FmtSt
    } = St,
    Fmt:terminate(FmtSt).


handle_call({queue, FileName}, _From, St) ->
    NewSt = queue(St, FileName),
    {reply, ok, NewSt};

handle_call({store, FileName, Line, Col, Code, Msg}, _From, St) ->
    NewSt = store(St, FileName, Line, Col, Code, Msg),
    {reply, ok, NewSt};

handle_call({finish, FileName}, _From, St) ->
    finish(St, FileName);

handle_call(wait, From, St) ->
    wait(St, From);

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_Vsn, St, _Extra) ->
    {ok, St}.


queue(#st{files = Files} = St, FileName) ->
    St#st{files = queue:in(FileName, Files)}.


store(#st{reports = Reports} = St, FileName, Line, Col, Code, Msg) ->
    NewReports = case dict:is_key(FileName, Reports) of
        true ->
            dict:append(FileName, {Line, Col, Code, Msg}, Reports);
        false ->
            dict:store(FileName, [{Line, Col, Code, Msg}], Reports)
    end,
    St#st{reports = NewReports}.


finish(#st{finished = Finished} = St, FileName) ->
    NewFinished = sets:add_element(FileName, Finished),
    drain(St#st{finished = NewFinished}).


wait(#st{waiter = Waiter} = St, From) when Waiter /= undefined ->
    gen_server:reply(From, {error, waiter_exists}),
    {noreply, St};

wait(#st{files = Files, count = Count} = St, From) ->
    case queue:is_empty(Files) of
        true ->
            gen_server:reply(From, {ok, Count}),
            {stop, normal, St};
        false ->
            {noreply, St#st{waiter = From}}
    end.


drain(St) ->
    #st{
        files = Files,
        finished = Finished
    } = St,
    case queue:peek(Files) of
        empty ->
            {reply, ok, St};
        {value, FileName} ->
            case sets:is_element(FileName, Finished) of
                true ->
                    format_report(St, FileName);
                false ->
                    {reply, ok, St}
            end
    end.


format_report(St, FileName) ->
    #st{
        files = Files,
        reports = Reports,
        finished = Finished,
        count = Count,
        formatter = Fmt,
        formatter_st = FmtSt,
        waiter = Waiter
    } = St,
    FileReports = case dict:find(FileName, Reports) of
        {ok, R} -> lists:sort(R);
        error -> []
    end,
    NewFmtSt = lists:foldl(fun({Line, Col, Code, Msg}, Acc) ->
        Fmt:format(FileName, Line, Col, Code, Msg, Acc)
    end, FmtSt, FileReports),
    {{value, FileName}, NewFiles} = queue:out(Files),
    NewReports = dict:erase(FileName, Reports),
    NewFinished = sets:del_element(FileName, Finished),
    NewCount = Count + length(FileReports),
    case queue:is_empty(NewFiles) of
        true when Waiter /= undefined ->
            gen_server:reply(Waiter, {ok, NewCount}),
            {stop, normal, St};
        _ ->
            NewSt = St#st{
                files = NewFiles,
                reports = NewReports,
                finished = NewFinished,
                formatter_st = NewFmtSt,
                count = NewCount
            },
            drain(NewSt)
    end.


get_formatter() ->
    Fmt = emilio_cfg:get(report_formatter),
    {_, Formatter} = if
        is_atom(Fmt) -> lists:keyfind(Fmt, 1, ?FORMATTERS);
        is_list(Fmt) -> lists:keyfind(list_to_atom(Fmt), 1, ?FORMATTERS)
    end,
    Formatter.
