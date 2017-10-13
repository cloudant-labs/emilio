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

-module(emilio_util).


-export([
    shutdown/1,
    time_it/3,
    get_priv/1
]).


shutdown(Status) ->
    erlang:halt(Status, [{flush, true}]).


time_it(Type, Key, Action) ->
    Start = os:timestamp(),
    try
        Action()
    after
        Diff = timer:now_diff(os:timestamp(), Start),
        emilio_report:update_stat(Type, Key, Diff)
    end.


get_priv(FileName) ->
    {module, zip} = code:ensure_loaded(zip),
    Script = escript:script_name(),
    {ok, [_, _, _, Body]} = escript:extract(Script, []),
    {archive, ArchiveBin} = Body,
    try
        zip:foldl(fun find_file/4, FileName, {Script, ArchiveBin}),
        undefined
    catch throw:{found, Data} ->
        {ok, Data}
    end.


find_file(FileName, _, GetBin, FileName) ->
    throw({found, GetBin()});

find_file(_, _, _, FileName) ->
    FileName.
