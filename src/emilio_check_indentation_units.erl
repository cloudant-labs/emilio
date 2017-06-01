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

-module(emilio_check_indentation_units).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [111].


explain(111) ->
    "White space is important".


format_error(111, Units) ->
    io_lib:format("leading white space should be a multiple of ~b", [Units]).


run(Lines) ->
    Units = emilio_cfg:get_int(indentation, units, 4),
    emilio_lib:foreach_line(fun(Loc, Line) ->
        check_line(Loc, Units, Line)
    end, Lines).


check_line(_Loc, _Units, [{white_space, _, [$\n]}]) ->
    ok;

check_line(Loc, Units, [{white_space, _, Text} | _]) ->
    if length(Text) rem Units == 0 -> ok; true ->
        ?EMILIO_REPORT(Loc, 111, Units)
    end;

check_line(_Loc, _UNits, _Line) ->
    ok.

