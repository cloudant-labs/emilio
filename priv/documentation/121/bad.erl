
ex_case(Animal) ->
    case Animal of
    cat ->
        evil;
    dog ->
        awesome
    end.


ex_fun() ->
    fun(Arg) when is_atom(Arg) ->
        Arg;
    (Arg) when is_list(Arg) ->
        list_to_existing_atom(Arg)
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
    end.


ex_try(Car) ->
    try
        start_engine(Car)
    catch
    throw:missing_keys ->
        missing_keys;
    throw:no_gas ->
        no_gas
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