
-module(good).


-export([
    doubles/0
]).


doubles() ->
    lists:map(fun(I) -> I * 2 end, lists:seq(1, 10)).
