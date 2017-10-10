good() ->
    A = [a, b, c],
    B  = [
        d,
        e,
        f
    ],
    A ++ B.


bad() ->
    A = [ a, b, c],
    B = [ d, e, f],
    A ++ B ++ [ ].
