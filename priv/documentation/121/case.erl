good(Animal) ->
    case Animal of
        cat ->
            evil;
        dog ->
            awesome
    end.


bad(Animal) ->
    case Animal of
    cat ->
        evil;
    dog ->
        awesome
    end.
