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

-module(emilio_check_ws_spaces_only).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [201, 202, 203, 204].


explain(201) ->
    "Tabs are bad, mmmkay!";
explain(202) ->
    "Form feeds are bad, mmmkay!";
explain(203) ->
    "Just Say No to Windows Line Endings.";
explain(204) ->
    "Don't use weird control or unicode code points in white space".


format_error(201, _) ->
    "white space contains tabs";
format_error(202, _) ->
    "white space contains carriage returns";
format_error(203, _) ->
    "line ending has carriage return";
format_error(204, Code) ->
    io_lib:format("line contains invalid control code: ~b", [Code]).


run(Lines) ->
    emilio_lib:foreach_token(fun check/3, Lines).


check(Anno, {white_space, Anno, Text}, _) ->
    check_ws(Anno, Text, $\t, 201),
    NewText = check_line_ending(Text, Anno),
    check_ws(Anno, NewText, $\r, 202),
    check_not_space(Anno, NewText);

check(_Anno, _Token, _Ctx) ->
    ok.


check_ws(_, [], _Char, _Code) ->
    ok;

check_ws(Anno, [Char | Rest], Char, Code) ->
    ?EMILIO_REPORT(Anno, Code, undefined),
    check_ws(emilio_anno:inc_col(Anno), Rest, Char, Code);

check_ws(Anno, [_ | Rest], Char, Code) ->
    check_ws(emilio_anno:inc_col(Anno), Rest, Char, Code).


check_line_ending(Text, Anno) when length(Text) >= 2 ->
    % Return a modified Text here removing
    % the trailing \r so that its not
    % reported as a 202 for line endings.
    Suffix = lists:nthtail(length(Text) - 2, Text),
    case Suffix of
        [$\r, $\n] ->
            ?EMILIO_REPORT(Anno, 203),
            lists:sublist(Text, length(Text) - 2);
        _ ->
            Text
    end;

check_line_ending(Text, _) ->
    Text.


check_not_space(_Anno, []) ->
    ok;

check_not_space(Anno, [C | Rest]) ->
    case lists:member(C, " \r\t\n") of
        true ->
            ok;
        false ->
            ?EMILIO_REPORT(Anno, 204, C)
    end,
    check_not_space(emilio_anno:inc_col(Anno), Rest).
