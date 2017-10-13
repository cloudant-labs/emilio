
oneline() ->
    A = [1, 2, 3, 4],
    lists:map(fun(D) -> D * D end,
            A).


oneline_clauses() ->
    A = [1, 2, 3, 4],
    lists:map(
            fun(D) when D < 3 -> true;
                (_) -> false
            end,
            A).


multiline() ->
    A = [1, 2, 3, 4],
    lists:map(
        fun(D) ->
            D * D
        end,
        A).


multiline_clauses() ->
    A = [1, 2, 3, 4],
    lists:map(
        fun(D) when D < 3 ->
            D * D;
            (D) when D >= 3 ->
                D + 2
        end,
        A).
