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

-module(emilio_check_indents_op2).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [124, 125].


explain(124) ->
    "indentation does not increase two levels after trailing operator";
explain(125) ->
    "indentation does not increase two levels with prefixed operator".


format_error(124, Op) ->
    Fmt = "indentation does not increase two levels for trailing ~s operator",
    io_lib:format(Fmt, [element(3, Op)]);
format_error(125, Op) ->
    Fmt = "indentation does not increase two levels for leading ~s operator",
    io_lib:format(Fmt, [element(3, Op)]).


run(Lines) ->
    Processed = lists:flatmap(fun process_line/1, Lines),
    check_indents(Processed).


check_indents([]) ->
    ok;

check_indents(Lines) ->
    case take_group(Lines) of
        {[_Line], Rest} ->
            check_indents(Rest);
        {Group, Rest} ->
            check_group_indents(Group),
            check_indents(Rest)
    end.


check_group_indents([{_, Op, Level} | Rest]) ->
    check_group_indents(Level, Op, Rest).


check_group_indents(_, _, []) ->
    ok;

check_group_indents(BaseLevel, Op, [Line | Rest]) ->
    LineLevel = element(3, Line),
    % Reports switch depending on whether we're looking
    % at a prefixed or suffixed operator.
    ReportCode = case Op of
        undefined -> 125;
        _ -> 124
    end,
    ReportOp = case Op of
        undefined -> element(1, Line);
        _ -> Op
    end,
    if LineLevel >= BaseLevel + 2 -> ok; true ->
        ?EMILIO_REPORT(element(2, ReportOp), ReportCode, ReportOp)
    end,
    check_group_indents(BaseLevel, element(2, Line), Rest).


process_line(Line) ->
    CodeOnly = lists:filter(fun is_code/1, Line),
    case CodeOnly of
        [] ->
            [];
        Tokens ->
            First = case hd(Tokens) of
                {op2, _, _} = T1 ->
                    T1;
                {match, Anno1} ->
                    {op2, Anno1, '='};
                _ ->
                    undefined
            end,
            Last = case lists:last(Tokens) of
                {op2, _, _} = T2 when T2 /= First ->
                    T2;
                {match, Anno2}
                        when First == undefined; Anno2 /= element(2, First) ->
                    {op2, Anno2, '='};
                _ ->
                    undefined
            end,
            [{First, Last, emilio_lib:indent_level(Line)}]
    end.


take_group([]) ->
    {[], []};

take_group([{_, undefined, _} = Line | Rest]) ->
    {Group, Tail} = take_group_if_prefix(Rest),
    {[Line | Group], Tail};

take_group([{_, {op2, _, _}, _} = Line | Rest]) ->
    {Group, Tail} = take_group(Rest),
    {[Line | Group], Tail}.


take_group_if_prefix([]) ->
    {[], []};

take_group_if_prefix([{undefined, _, _} | _] = Lines) ->
    {[], Lines};

take_group_if_prefix([{_, undefined, _} = Line | Rest]) ->
    {Group, Tail} = take_group_if_prefix(Rest),
    {[Line | Group], Tail};

take_group_if_prefix([{_, {op2, _, _}, _}  = Line | Rest]) ->
    {Group, Tail} = take_group(Rest),
    {[Line | Group], Tail}.


is_code({white_space, _, _}) -> false;
is_code({comment, _, _}) -> false;
is_code(_) -> true.
