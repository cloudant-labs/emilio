
ex_case(Arg) ->
    case Arg of
        {N, _} = AReallyLongPatternMatchThing
            when is_integer(N), N >= 0, N < 1073741824 ->
            ok;
        _ ->
            nope
    end.


ex_function(Arg)
    when is_atom(Arg) ->
    ok.
