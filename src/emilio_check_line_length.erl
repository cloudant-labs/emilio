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

-module(emilio_check_line_length).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [501].


explain(501) ->
    "Long lines suck.".


format_error(501, MaxLength) ->
    io_lib:format("line longer than ~b characters", [MaxLength]).


run(Lines) ->
    MaxLength = emilio_cfg:get_int(line_length, max, 80),
    emilio_lib:foreach_line(fun(Anno, Line) ->
        check_line(Anno, MaxLength, Line)
    end, Lines).


check_line(Anno, MaxLength, Line) ->
    LineTooLong = line_length(Line) > MaxLength,
    HasLongString = has_long_string(MaxLength, Line),
    if not LineTooLong orelse HasLongString -> ok; true ->
        ?EMILIO_REPORT(Anno, 501, MaxLength)
    end.


has_long_string(MaxLength, Line) ->
    % Sometimes source code will have a URL or other unbreakable
    % long bit of text. We allow for anything that doesn't
    % start within 10% of the max line length.
    StartLen = max(0, MaxLength - round(MaxLength * 0.1)),
    lists:foldl(fun(Tok, HasLong) ->
        case Tok of
            {Name, Anno, Text} when Name == string; Name == comment ->
                {_Line, Col} = emilio_anno:lc(Anno),
                if Col >= StartLen -> HasLong; true ->
                    HasLong orelse check_long(MaxLength, Col, Text)
                end;
            _ ->
                HasLong
        end
    end, false, Line).


check_long(MaxLength, StartCol, Text) ->
    TextToks = string:tokens(Text, " \t\r"),
    lists:foldl(fun(TT, HasLong) ->
        case length(TT) > (MaxLength - StartCol) of
            true ->
                true;
            false ->
                HasLong
        end
    end, false, TextToks).


line_length(Line) ->
    case lists:last(Line) of
        {white_space, Anno, WS} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            Col + length(WS) - 1;
        {dot, Anno} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            Col
    end.
