good(Animal) ->
    if
        Animal == cat ->
            boo;
        Animal == bear ->
            run_away;
        true ->
            pet_it
    end.


bad(Animal) ->
    if
    Animal == cat ->
        boo;
    Animal == bear ->
        run_away;
    true ->
        pet_it
    end.
