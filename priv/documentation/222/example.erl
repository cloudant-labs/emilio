good() ->
    A = [1, 2, 3],
    B = lists:map(fun(D) -> D * D end, A),
    lists:sum(B).


bad() ->
    A = [1,2,3],B = lists:map(fun(D) -> D * D end,A),lists:sum(B).
