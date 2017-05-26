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

-module(emilio_lib).


-export([
    report/4,

    foreach_line/2,
    foreach_token/2,

    line_length/1
]).


report(Module, {Line, Col}, Code, Arg) ->
    Msg = Module:format_error(Code, Arg),
    Fmt = "~b : ~b(~b) : ~s~n",
    io:format(Fmt, [Code, Line, Col, Msg]).


foreach_line(UserFun, Lines) ->
    lists:foreach(fun(Line) ->
        Loc = element(2, hd(Line)),
        UserFun(Loc, Line)
    end, Lines).


foreach_token(UserFun, Lines) ->
    lists:foreach(fun(Line) ->
        lists:foreach(fun(Token) ->
            Loc = element(2, Token),
            UserFun(Loc, Token)
        end, Line)
    end, Lines).


line_length(Line) ->
    case lists:last(Line) of
        {white_space, {_LineNum, Col}, WS} ->
            Col + length(WS) - 1;
        {dot, {_LineNum, Col}} ->
            Col
    end.
