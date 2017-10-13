
oneline() ->
    A = [1, 2, 3, 4],
    lists:map(fun(D) -> D * D end, A).


oneline_clauses() ->
    A = [1, 2, 3, 4],
    lists:map(fun(D) when D < 3 -> true; (_) -> false end, A).


multiline() ->
    A = [1, 2, 3, 4],
    lists:map(fun(D) ->
        D * D
    end, A).


multiline_clauses() ->
    A = [1, 2, 3, 4],
    lists:map(fun
        (D) when D < 3 ->
            D * D;
        (D) when D >= 3 ->
            D + 2
    end, A).


with_var() ->
    A = [1, 2, 3, 4],
    B = fun(D) ->
        D * D
    end,
    lists:map(B, A).


with_name() ->
    A = [1, 2, 3, 4],
    Square = fun Square(D) ->
        D * D
    end,
    lists:map(Square, A).
