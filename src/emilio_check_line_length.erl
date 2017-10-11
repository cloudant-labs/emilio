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
    format_error/2,
    run/1
]).


-include("emilio.hrl").


format_error(501, MaxLength) ->
    io_lib:format("line longer than ~b characters", [MaxLength]).


run(Lines) ->
    MaxLength = emilio_cfg:get(max_line_length),
    emilio_lib:foreach_line(fun(Anno, Line) ->
        check_line(Anno, MaxLength, Line)
    end, Lines).


check_line(Anno, MaxLength, Line) ->
    LineTooLong = is_long_line(MaxLength, Line),
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


is_long_line(MaxLength, Line) ->
    case lists:last(Line) of
        {atom, Anno, Name} ->
            % Multiline atoms are possible if not totes
            % icky.
            check_multiline_string(MaxLength, Anno, atom_to_list(Name)),
            false;
        {string, Anno, Text} ->
            % Multiline strings show up as the last token
            % of the line they started on. Since we have
            % to check manually we short circuit the general
            % check by returning false here.
            check_multiline_string(MaxLength, Anno, Text),
            false;
        {bin_tsl, _, _} ->
            % Similar to multiline strings, we're looking
            % for a multiline bin element initializer so
            % will report the error on our own
            check_bin_expr(MaxLength, Line),
            false;
        {comment, Anno, Text} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            (Col + length(Text)) > MaxLength;
        {char, Anno, _} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            Col > MaxLength;
        {white_space, Anno, WS} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            (Col + length(WS) - 1) > MaxLength;
        {span_end, _} ->
            is_long_line(MaxLength, lists:sublist(Line, length(Line) - 1));
        {dot, Anno} ->
            {_Line, Col} = emilio_anno:lc(Anno),
            Col > MaxLength
    end.


check_multiline_string(_, _, []) ->
    ok;

check_multiline_string(MaxLength, Anno, [C | Rest]) ->
    {_, Col} = emilio_anno:lc(Anno),
    case Col + 1 > MaxLength of
        true ->
            % Just report the first instance for now
            ?EMILIO_REPORT(Anno, 501, MaxLength);
        false when C == $\n ->
            NewAnno = emilio_anno:inc_line(Anno),
            check_multiline_string(MaxLength, NewAnno, Rest);
        false ->
            NewAnno = emilio_anno:inc_col(Anno),
            check_multiline_string(MaxLength, NewAnno, Rest)
    end.


check_bin_expr(MaxLength, Line) ->
    CheckToken = fun(Tok) ->
        Anno = element(2, Tok),
        {_, Col} = emilio_anno:lc(Anno),
        if Col =< MaxLength -> ok; true ->
            ?EMILIO_REPORT(Anno, 501, MaxLength)
        end
    end,
    RevLine = lists:reverse(Line),
    case RevLine of
        [{bin_tsl, _, _}, {bin_size, _, _} | Rest] ->
            % Getting the expression of the element
            Pred = fun(T) -> element(1, T) /= bin_element end,
            Prefix = lists:takewhile(Pred, Rest),
            case Prefix of
                [{string, StrAnno, Text}] ->
                    check_multiline_string(MaxLength, StrAnno, Text);
                [Tok | _] ->
                    CheckToken(Tok)
            end;
        [{bin_tsl, _, _} = T | _] ->
            CheckToken(T)
    end.
