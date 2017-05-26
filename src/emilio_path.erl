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

-module(emilio_path).


-export([
    walk/2
]).


walk(FilePath, UserFun) ->
    case filelib:is_dir(FilePath) of
        true ->
            walk_dir(FilePath, UserFun);
        false ->
            UserFun(FilePath)
    end.


walk_dir(DirPath, UserFun) ->
    Pattern = filename:join([DirPath, "*"]),
    Files = filelib:wildcard(Pattern),
    NonDot = lists:filter(fun(F) -> hd(F) /= "." end, Files),
    RegularFiles = lists:filter(fun filelib:is_regular/1, NonDot),
    lists:foreach(fun(File) ->
        walk(File, UserFun)
    end, lists:sort(RegularFiles)),
    Dirs = lists:filter(fun filelib:is_dir/1, NonDot),
    lists:foreach(fun(Dir) ->
        walk(Dir, UserFun)
    end, lists:sort(Dirs)).
