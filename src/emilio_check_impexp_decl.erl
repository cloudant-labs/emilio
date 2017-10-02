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

-module(emilio_check_impexp_decl).

-export([
    codes/0,
    explain/1,
    format_error/2,
    run/1
]).


-include("emilio.hrl").


codes() ->
    [401, 402].


explain(401) ->
    "Only module and behavior attributes are allowed before exports";
explain(402) ->
    "Imports should not be used".


format_error(401, Name) ->
    io_lib:format("invalid ~s attribute before exports", [Name]);
format_error(402, _) ->
    "import declaration found".


run(Lines) ->
    Attrs = find_attributes(Lines),
    check_order(Attrs),
    check_no_imports(Attrs).


check_order(Attrs) ->
    Names = [element(3, Attr) || Attr <- Attrs],
    HasExport = lists:member(export, Names),
    Allowed = [module, behavior, behaviour],
    DropFun = fun(Attr) -> lists:member(element(3, Attr), Allowed) end,
    RestAttrs = lists:dropwhile(DropFun, Attrs),
    case RestAttrs of
        [{attribute, _, export} | _] ->
            ok;
        [BadAttr | _] when HasExport ->
            ?EMILIO_REPORT(BadAttr, 401, element(3, BadAttr));
        _ ->
            ok
    end.


check_no_imports(Attrs) ->
    lists:foreach(fun(Attr) ->
        if element(3, Attr) /= import -> ok; true ->
            ?EMILIO_REPORT(Attr, 402)
        end
    end, Attrs).


find_attributes(Lines) ->
    RevAttrs = emilio_lib:fold_tokens(fun(Token, _, Acc) ->
        case element(1, Token) == attribute of
            true ->
                {continue, [Token | Acc]};
            false ->
                {continue, Acc}
        end
    end, [], Lines),
    lists:reverse(RevAttrs).
