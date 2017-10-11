
ex_case(Car) ->
    case start_ignition(Car) of
    started -> ok;
    {not_started, Reason} when Reason == ford ->
            obviously;
            Else ->
                Else
    end.


ex_function(Atom) ->
            why;
ex_function(Atom)
            when is_integer(Other) ->
                also_why.
