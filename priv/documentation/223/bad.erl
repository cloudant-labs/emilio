
bad(foo) ->
    ok ;
bad(Other) ->
    case Other of
        bar -> ok
        ; baz -> not_even_once
    end.
