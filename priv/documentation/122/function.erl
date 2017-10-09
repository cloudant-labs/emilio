good(Arg) when is_atom(Arg) ->
    ok;

good(Arg)
        when is_integer(Arg), Arg >= 0, Arg =< 1073741824 ->
    ok.


bad(Arg)
    when is_atom(Arg) ->
    ok.
