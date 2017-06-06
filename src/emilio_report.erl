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
    start/0,
    queue/1,
    store/5,
    finish/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    files = queue:new(),
    reports = dict:new(),
    finished = sets:new()
}).


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).


queue(FileName) ->
    gen_server:call(?MODULE, {queue, FileName}).


store(FileName, Module, Anno, Code, Arg) ->
    {Line, Col} = emilio_anno:lc(Anno),
    Msg = Module:format_error(Code, Arg),
    gen_server:call(?MODULE, {store, FileName, Line, Col, Code, Msg}).


finish(FileName) ->
    gen_server:call(?MODULE, {finish, FileName}).


init(_) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({queue, FileName}, _From, St) ->
    NewSt = queue(St, FileName),
    {reply, ok, NewSt};

handle_call({store, FileName, Line, Col, Code, Msg}, _From, St) ->
    NewSt = store(St, FileName, Line, Col, Code, Msg),
    {reply, ok, NewSt};

handle_call({finish, FileName}, _From, St) ->
    NewSt = finish(St, FileName),
    {reply, ok, NewSt};

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


drain(St) ->
    #st{
        files = Files,
        reports = Reports,
        finished = Finished
    } = St,
    case queue:peek(Files) of
        empty ->
            St;
        {value, FileName} ->
            case sets:is_element(FileName, Finished) of
                true ->
                    FileReports = case dict:find(FileName, Reports) of
                        {ok, R} -> R;
                        error -> []
                    end,
                    render(FileName, FileReports),
                    {{value, FileName}, NewFiles} = queue:out(Files),
                    NewReports = dict:erase(FileName, Reports),
                    NewFinished = sets:del_element(FileName, Finished),
                    St#st{
                        files = NewFiles,
                        reports = NewReports,
                        finished = NewFinished
                    };
                false ->
                    St
            end
    end.


render(_FileName, []) ->
    ok;

render(FileName, Reports) ->
    Formatter = get_formatter(),
    lists:foreach(fun({Line, Col, Code, Msg}) ->
        Formatter(FileName, Line, Col, Code, Msg)
    end, lists:sort(Reports)).


get_formatter() ->
    fun default_formatter/5.


default_formatter(FileName, Line, Col, Code, Msg) ->
    io:format("~s:~b(~b) - ~b - ~s~n", [FileName, Line, Col, Code, Msg]).
