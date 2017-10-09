good(Car) ->
    case start_ignition(Car) of
        started ->
            ok;
        {not_started, Reason} when Reason == ford ->
            obviously;
        Else ->
            Else
    end.


bad(Car) ->
    case start_ignition(Car) of
    started -> ok;
    {not_started, Reason} when Reason == ford ->
            obviously;
            Else ->
                Else
    end.
