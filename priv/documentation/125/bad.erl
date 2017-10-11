
ex_case(Animal) ->
    case Animal of
        chicken ->
        eggs;
        cat ->
        icky_kittens;
        dog ->
        puppies
    end.


ex_fun() ->
    fun(Arg) ->
    ok
    end.


ex_function() ->
ok.


ex_if(Arg) ->
    if
        Arg == cat ->
        do_not_adopt;
        Arg == dog ->
        your_new_best_friend
    end.


ex_receive() ->
    receive
        Msg ->
        received;
        _Other ->
        not_received
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
        obviously;
        Else ->
        Else
    catch
        throw:missing_keys ->
        missing_keys;
        throw:no_gas ->
        no_gas
    end.
