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

-module(emilio_docs).


-export([
    list_codes/0,
    explain_code/1
]).


list_codes() ->
    io:format("~nError Codes~n===========~n~n", []),
    lists:foldl(fun({Code, Subject, _}, Acc) ->
        NewAcc = Code div 100,
        if NewAcc == Acc -> ok; true ->
            io:format("~n", [])
        end,
        io:format("  ~b - ~s~n", [Code, Subject]),
        NewAcc
    end, 1, gather_codes()),
    io:format("~n", []),
    emilio_util:shutdown(0).


explain_code(Code) ->
    Codes = gather_codes(),
    case lists:keyfind(Code, 1, Codes) of
        {Code, _, Descr} ->
            io:format("~n~b - ~s~n", [Code, Descr]),
            emilio_util:shutdown(0);
        _ ->
            io:format("Unknown code: ~b~n", [Code]),
            emilio_util:shutdown(1)
    end.


gather_codes() ->
    {module, zip} = code:ensure_loaded(zip),
    Script = escript:script_name(),
    {ok, [_, _, _, Body]} = escript:extract(Script, []),
    {archive, ArchiveBin} = Body,
    {ok, Codes} = zip:foldl(fun load_codes/4, [], {Script, ArchiveBin}),
    lists:sort(Codes).


load_codes(FileName, _, GetBin, Codes) ->
    case filename:basename(FileName) of
        "description.md" ->
            CodeStr = filename:basename(filename:dirname(FileName)),
            Code = list_to_integer(CodeStr),
            Contents = GetBin(),
            [Subject | _] = binary:split(Contents, <<"\n">>),
            [{Code, Subject, Contents} | Codes];
        _ ->
            Codes
    end.
