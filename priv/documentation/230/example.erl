good(Arg) ->
    Var = do_something(Arg),
    (Var orelse true).


bad( Arg) ->
    Var = do_something( Arg),
    ( Var orelse true).
