-module(bad).

-export([
    doubles/0
]).


-import(lists, [
    map/2,
    seq/2
]).


doubles() ->
    map(fun(I) -> I * 2 end, seq(1, 10)).
