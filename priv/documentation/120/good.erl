
ex_case(Animal) ->
    case Animal of
        spider ->
            scary;
        bunny ->
            not_scary
    end.


ex_if(Animal) ->
    if
        Animal == cat ->
            boo;
        Animal == bear ->
            run_away;
        true ->
            pet_it
    end.


ex_receive(Msg) ->
    receive
        Msg ->
            received;
        _Other ->
            not_received
    after 1000 ->
        got_bored
    end.


ex_try(Car) ->
    try
        start_engine(Car)
    catch throw:missing_keys ->
        missing_keys
    end.


ex_try_of(Car) ->
    try start_engine(Car) of
        started ->
            started;
        {not_started, Reason} when Reason == ford ->
            obviously
    catch throw:missing_keys ->
        missing_keys
    end.
