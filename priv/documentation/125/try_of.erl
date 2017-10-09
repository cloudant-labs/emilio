good(Car) ->
    try start_engine(Car) of
        started ->
            started;
        {not_started, Reason} when Reason == ford ->
            obviously;
        Else ->
            Else
    catch
        throw:missing_keys ->
            missing_keys;
        throw:no_gas ->
            no_gas
    end.


bad(Car) ->
    try start_engine(Car) of
        started ->
        started;
        {not_started, Reason} when Reason == ford ->
        obviously;
        Else ->
        Else
    catch
        throw:missing_keys ->
        missing_keys;
        throw:no_gas ->
        no_gas
    end.
