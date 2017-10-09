good(Animal) ->
    case Animal of
        chicken ->
            eggs;
        cat ->
            icky_kittens;
        dog ->
            puppies
    end.


bad(Animal) ->
    case Animal of
        chicken ->
        eggs;
        cat ->
        icky_kittens;
        dog ->
        puppies
    end.
