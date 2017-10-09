good(Arg) ->
    case Arg of
        {N, _} = AReallyLongPatternMatchThing
                when is_integer(N), N >= 0, N < 1073741824 ->
            ok;
        _ ->
            nope
    end.


bad(Arg) ->
    case Arg of
        {N, _} = AReallyLongPatternMatchThing
            when is_integer(N), N >= 0, N < 1073741824 ->
            ok;
        _ ->
            nope
    end.
