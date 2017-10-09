good(Car) ->
    try
        start_engine(Car)
    catch throw:missing_keys ->
        missing_keys
    end.


bad(Car) ->
    try
        start_engine(Car)
        catch throw:missing_keys ->
            missing_keys
        end.
