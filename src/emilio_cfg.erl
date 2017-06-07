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

-module(emilio_cfg).


-export([
    compile/1,
    get/1
]).


-define(TABLE, emilio_config).

-define(IGNORE_OPTS, [
    help
]).

-define(VALIDATORS, [
    {pos_integer, fun pos_integer/1},
    {file, fun file/1},
    {formatter, fun formatter/1}
]).

-define(FORMATTERS, [
    text, "text",
    summary, "summary",
    csv, "csv",
    json, "json"
]).


compile(Opts) ->
    ets:new(?TABLE, [set, named_table, protected]),
    {ok, BaseConfigBin} = emilio_util:get_priv("emilio.config"),
    {ok, BaseConfig} = consult_bin(BaseConfigBin),
    {ok, FileConfig} = case lists:keyfind(config, 1, Opts) of
        {config, FileName} ->
            file:consult(FileName);
        _ ->
            {ok, []}
    end,
    compile(BaseConfig, FileConfig, Opts),
    ok.


get(Name) ->
    [{Name, Value}] = ets:lookup(?TABLE, Name),
    Value.


compile(BaseConfig, FileConfig, Opts) ->
    KVs1 = [{K, V} || {K, _, V, _} <- BaseConfig],
    KVs2 = lists:foldl(fun({K, V}, Acc) ->
        lists:keystore(K, 1, Acc, {K, V})
    end, KVs1, FileConfig),
    KVs3 = lists:foldl(fun({K, V}, Acc) ->
        case lists:member(K, ?IGNORE_OPTS) of
            true ->
                Acc;
            false ->
                lists:keystore(K, 1, Acc, {K, V})
        end
    end, KVs2, Opts),
    lists:foreach(fun({K, V}) -> validate(K, V, BaseConfig) end, KVs3),
    lists:foreach(fun({K, V}) -> ets:insert(?TABLE, {K, V}) end, KVs3).


validate(K, V, BaseConfig) ->
    case lists:keyfind(K, 1, BaseConfig) of
        {K, ValidatorName, _, _} ->
            {_, Fun} = lists:keyfind(ValidatorName, 1, ?VALIDATORS),
            case Fun(V) of
                true ->
                    ok;
                false ->
                    emilio_log:error("Invalid value ~p for ~p~n", [V, K]),
                    emilio_util:shutdown(1)
            end;
        _ ->
            emilio_log:warn("Unknown config option: ~p~n", [K])
    end.


pos_integer(I) when is_integer(I), I > 0 ->
    true;
pos_integer(_) ->
    false.


file(undefined) ->
    true;
file(S) when is_list(S) ->
    filelib:is_file(S);
file(B) when is_binary(B) ->
    file(binary_to_list(B));
file(_) ->
    false.


formatter(Name) ->
    lists:member(Name, ?FORMATTERS).


consult_bin(Bin) when is_binary(Bin) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
    Forms = split_forms(Tokens),
    {ok, lists:map(fun(Form) ->
        {ok, Term} = erl_parse:parse_term(Form),
        Term
    end, Forms)}.


split_forms([]) ->
    [];

split_forms(Tokens) ->
    {Form, Rest} = split_form(Tokens),
    [Form | split_forms(Rest)].


split_form([]) ->
    [];

split_form([{dot, _} = Token | Rest]) ->
    {[Token], Rest};

split_form([Token | Rest]) ->
    {RestForm, RestTokens} = split_form(Rest),
    {[Token | RestForm], RestTokens}.
