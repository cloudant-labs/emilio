good(Car) ->
    try start_engine(Car) of
        started ->
            started;
        {not_started, Reason} when Reason == ford ->
            obviously
    catch throw:missing_keys ->
        missing_keys
    end.


bad(Car) ->
    try start_engine(Car)
        of
            started ->
                started;
            {not_started, Reason} when Reason == ford ->
                obviously
        catch throw:missing_keys ->
            missing_keys
        end.
