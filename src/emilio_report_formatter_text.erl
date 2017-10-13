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

-module(emilio_report_formatter_text).


-export([
    init/0,
    terminate/3,
    format/6
]).


init() ->
    {ok, nil}.


terminate(_FileCount, _ErrorCount, _St) ->
    case emilio_cfg:get(debug_stats) of
        true ->
            display_timing();
        false ->
            ok
    end.


format(FileName, Line, Col, Code, Msg, _St) ->
    io:format("~s:~b(~b) - ~b - ~s~n", [FileName, Line, Col, Code, Msg]),
    case emilio_cfg:get(context) of
        0 ->
            ok;
        Count ->
            display_context(FileName, Line, Count)
    end.


display_context(FileName, Line, Count) ->
    {ok, Lines} = emilio_report:get_file(FileName),
    MinLine = erlang:max(1, Line - Count),
    MaxLine = erlang:min(Line + Count, length(Lines)),
    ContextLines = lists:sublist(Lines, MinLine, MaxLine - MinLine + 1),
    io:format("~n", []),
    lists:foreach(fun({LineNum, ContextLine}) ->
        Sep = case LineNum of
            Line -> "=>";
            _ -> ": "
        end,
        io:format("~6b ~s ~s~n", [LineNum, Sep, ContextLine])
    end, lists:zip(lists:seq(MinLine, MaxLine), ContextLines)),
    io:format("~n", []).


display_timing() ->
    io:format("~n", []),
    AllStats = emilio_report:get_stats(),

    Ops = [{OpName, Time, Count} || {op, OpName, Time, Count} <- AllStats],
    display_ops(Ops),

    Files = [{FileName, Time} || {file, FileName, Time, 1} <- AllStats],
    display_files(Files).


display_ops(Ops) ->
    Processed = lists:map(fun({Name, Time, _Count}) ->
        Total = Time / 1000,
        {Total, atom_to_list(Name)}
    end, Ops),
    Ordered = lists:reverse(lists:sort(Processed)),
    display_table(Ordered, "Operation").


display_files(Files) ->
    Processed = lists:map(fun({Name, Time}) ->
        Total = Time / 1000,
        {Total, Name}
    end, Files),
    Ordered = lists:reverse(lists:sort(Processed)),
    display_table(Ordered, "File Name").


display_table(Ordered, Column) ->
    MaxLen = lists:max([length(element(2, Row)) || Row <- Ordered]),
    HdrFmt = io_lib:format("  ~~-~b.s  ~~10.s (ms)~n", [MaxLen]),
    Hdr = io_lib:format(lists:flatten(HdrFmt), [Column, "Total Time"]),
    Sep = ["=" || _ <- lists:seq(1, length(lists:flatten(Hdr)) - 2)],
    io:format("~s  ~s~n~n", [Hdr, Sep]),
    RawFmt = io_lib:format("  ~~-~b.s  ~~10.3f~n", [MaxLen]),
    Fmt = lists:flatten(RawFmt),
    lists:foreach(fun({Total, Name}) ->
        io:format(Fmt, [Name, Total])
    end, Ordered),
    io:format("~n~n", []).
