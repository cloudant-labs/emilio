

ex_case_1(Animal) ->
    case Animal of
        spider ->
            scary;
        bunny ->
            not_scary
    end.


ex_case_2(Animal) ->
    case Animal of
        spider -> scary;
        bunny -> not_scary
    end.


ex_if_1(Animal) ->
    if
        Animal == cat ->
            boo;
        Animal == bear ->
            run_away;
        true ->
            pet_it
    end.


ex_if_2(Animal) ->
    if
        Animal == cat -> boo;
        Animal == bear -> run_away;
        true -> pet_it
    end.


ex_function_1(foo) ->
    baz;
ex_function_1(bar) ->
    blorp.


ex_function_2(foo) -> baz;
ex_function_2(bar) -> blorp.
