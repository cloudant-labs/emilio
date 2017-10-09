good(Car) ->
    try
        start_engine(Car)
    catch
        throw:missing_keys ->
            missing_keys;
        throw:no_gas ->
            no_gas
    end.


bad(Car) ->
    try
        start_engine(Car)
    catch
    throw:missing_keys ->
        missing_keys;
    throw:no_gas ->
        no_gas
    end.
