good(Atom) when is_atom(Atom) ->
    {ok, Atom};

good(Other)
        when is_integer(Other), Other >= 4, Other < 5 ->
    {also_ok, Other}.


bad(Atom) ->
            why;
bad(Atom)
            when is_integer(Other) ->
                also_why.
