
ex_case(Car) ->
    case start_ignition(Car) of
        started ->
            ok;
        {not_started, Reason} when Reason == ford ->
            obviously;
        Else ->
            Else
    end.


ex_function(Atom) when is_atom(Atom) ->
    {ok, Atom};

ex_function(Other)
        when is_integer(Other), Other >= 4, Other < 5 ->
    {also_ok, Other}.
