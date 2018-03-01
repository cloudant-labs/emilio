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

-module(emilio_check_exports_groups).

-export([
    format_error/2,
    run/1
]).


-include("emilio.hrl").

-define(BEHAVIORS, [
    {application, [
        {start, 2},
        {stop, 1}
    ]},
    {gen_event, [
        {init, 2},
        {terminate, 2},
        {handle_event, 2},
        {handle_call, 2},
        {handle_info, 2},
        {format_status, 2},
        {code_change, 3}
    ]},
    {gen_server, [
        {init, 1},
        {terminate, 2},
        {handle_call, 3},
        {handle_cast, 2},
        {handle_info, 2},
        {format_status, 2},
        {code_change, 3}
    ]},
    {supervisor, [
        {init, 1}
    ]}
]).

-define(OPTIONAL_CALLBACKS, [
    {gen_event, [
        {terminate, 2},
        {handle_info, 2},
        {format_status, 2},
        {code_change, 3}
    ]},
    {gen_server, [
        {terminate, 2},
        {handle_info, 2},
        {format_status, 2},
        {code_change, 3}
    ]}
]).


format_error(410, {NumBehaviors, NumExports}) ->
    Fmt = "there should be ~b to ~b export groups, not ~b",
    io_lib:format(Fmt, [NumBehaviors, NumBehaviors + 2, NumExports]);
format_error(411, {NumBehaviors, NumExports}) ->
    Fmt = "there should be ~b to ~b export groups, not ~b",
    io_lib:format(Fmt, [NumBehaviors, NumBehaviors + 2, NumExports]);
format_error(412, Behavior) ->
    Fmt = "~s export group not ordered correctly",
    io_lib:format(Fmt, [Behavior]);
format_error(413, Behavior) ->
    Fmt = "~s callbacks not ordered correctly",
    io_lib:format(Fmt, [Behavior]).


run(Lines) ->
    Behaviors = find_behaviors(Lines),
    Exports = find_exports(Lines),
    case check_counts(length(Behaviors), length(Exports)) of
        ok ->
            check_behaviors(1, Behaviors, Exports, false);
        {ok, Offset} ->
            check_behaviors(1 + Offset, Behaviors, Exports, true);
        error ->
            ok
    end.


check_counts(NumBehaviors, NumExports) when NumBehaviors > NumExports ->
    ?EMILIO_REPORT([{line, 0}, {column, 0}], 410, {NumBehaviors, NumExports}),
    error;
check_counts(NumBehaviors, NumExports) when NumBehaviors + 2 < NumExports ->
    ?EMILIO_REPORT([{line, 0}, {column, 0}], 411, {NumBehaviors, NumExports}),
    error;
check_counts(NumBehaviors, NumExports) when NumBehaviors == NumExports ->
    {ok, 0};
check_counts(NumBehaviors, NumExports) when NumBehaviors + 1 == NumExports ->
    ok;
check_counts(NumBehaviors, NumExports) when NumBehaviors + 2 == NumExports ->
    {ok, 1}.


check_behaviors(_, [], _, _) ->
    ok;

check_behaviors(Idx, [{Anno, Behavior} | RestBehaviors], Exports, Strict) ->
    case lists:keymember(Behavior, 1, ?BEHAVIORS) of
        true ->
            check_behavior(Idx, Anno, Behavior, Exports, Strict);
        false ->
            ok
    end,
    check_behaviors(Idx + 1, RestBehaviors, Exports, Strict).


check_behavior(Idx, BehaviorAnno, Behavior, Exports, Strict) ->
    {Anno1, Group1} = lists:nth(Idx, Exports),
    Expected = expected_callbacks(Behavior, Group1),
    case is_behavior(Group1, Expected) of
        true ->
            if Group1 == Expected -> ok; true ->
                ?EMILIO_REPORT(Anno1, 413, Behavior)
            end;
        false when not Strict andalso Idx + 1 =< length(Exports) ->
            {Anno2, Group2} = lists:nth(Idx + 1, Exports),
            Expected2 = expected_callbacks(Behavior, Group2),
            case is_behavior(Group2, Expected2) of
                true ->
                    if Group2 == Expected2 -> ok; true ->
                        ?EMILIO_REPORT(Anno2, 413, Behavior)
                    end;
                false ->
                    ?EMILIO_REPORT(BehaviorAnno, 412, Behavior)
            end;
        false ->
            ?EMILIO_REPORT(BehaviorAnno, 412, Behavior)
    end.


find_behaviors(Lines) ->
    Behaviors = emilio_lib:fold_tokens(fun(Token, _, Acc) ->
        case element(1, Token) == attribute of
            true ->
                case element(3, Token) of
                    behavior ->
                        Behavior = {element(2, Token), element(4, Token)},
                        {continue, [Behavior | Acc]};
                    behaviour ->
                        Behavior = {element(2, Token), element(4, Token)},
                        {continue, [Behavior | Acc]};
                    _ ->
                        {continue, Acc}
                end;
            false ->
                {continue, Acc}
        end
    end, [], Lines),
    lists:reverse(Behaviors).


find_exports(Lines) ->
    Exports = emilio_lib:fold_tokens(fun(Token, _, Acc) ->
        case element(1, Token) of
            attribute ->
                case element(3, Token) of
                    export ->
                        {continue, [{element(2, Token), []} | Acc]};
                    _ ->
                        {continue, Acc}
                end;
            export ->
                Export = {element(3, Token), element(4, Token)},
                [{GroupAnno, CurrGroup} | RestGroups] = Acc,
                {continue, [{GroupAnno, [Export | CurrGroup]} | RestGroups]};
            _ ->
                {continue, Acc}
        end
    end, [], Lines),
    lists:reverse(lists:map(fun reverse_group/1, Exports)).


reverse_group({Anno, Exports}) ->
    {Anno, lists:reverse(Exports)}.


is_behavior(Found, Expected) ->
    lists:sort(Found) == lists:sort(Expected).


expected_callbacks(Behavior, Provided) ->
    {_, AllCallbacks} = lists:keyfind(Behavior, 1, ?BEHAVIORS),
    case lists:keyfind(Behavior, 1, ?OPTIONAL_CALLBACKS) of
        {Behavior, OptionalCallbacks} ->
            lists:filter(fun(Callback) ->
                not lists:member(Callback, OptionalCallbacks) orelse
                        lists:member(Callback, Provided)
            end, AllCallbacks);
        false ->
            AllCallbacks
    end.
