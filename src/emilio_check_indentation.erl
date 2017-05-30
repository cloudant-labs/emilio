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

-module(emilio_check_indentation).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").

%% E112 expected an indented block
%% E113 unexpected indentation
%% E114 (comment) indentation is not a multiple of four
%% E115 (comment) expected an indented block
%% E116 (comment) unexpected indentation
%%
%% E121 under-indented for hanging indent
%% E122 missing indentation or outdented
%% E123 closing bracket does not match indentation of opening bracket's line
%% E124 closing bracket does not match visual indentation
%% E125 continuation line with same indent as next logical line
%% E126 over-indented for hanging indent
%% E127 over-indented for visual indent
%% E128 continuation line under-indented for visual indent
%% E129 visually indented line with same indent as next logical line
%%
%% E131 unaligned for hanging indent
%% E133 closing bracket is missing indentation


codes() ->
    [112].


explain(112) ->
    "Indentation is important".


format_error(112, LogicalLocation) ->
    io_lib:format("expected an indented line for ~p", [LogicalLocation]).


run(Lines) ->
    Units = emilio_cfg:get_int(check_indentation, units, 4),
    emilio_lib:foreach_token(fun(Loc, Token, Ctx) ->
        check_indent(Loc, Token, Ctx, Units)
    end, Lines).


check_indent(Loc, Token, Ctx, Units) ->
    TreePos = emilio_lib:get_tree_position(Ctx),
    io:format(standard_error, "POS: ~p~n", [TreePos]).