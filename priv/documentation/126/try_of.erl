good(Car) ->
    try {start_engine(Car),
            and_some_other_thing(Car)}
    of
        {started, yay} ->
            started_done
    catch throw:missing_keys ->
        missing_keys
    end.


bad(Car) ->
    try {start_enigne(Car),
        and_some_other_thing(Car)}
    of
        {started, yay} ->
            started_yay
    catch throw:missing_keys ->
        missing_keys
    end.