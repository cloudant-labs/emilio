good(Animal) ->
    case Animal of
        spider ->
            scary;
        bunny ->
            not_scary
    end.


bad(Animal) ->
    case Animal
        of
        spider ->
            scary;
        bunny ->
            not_scary
        end.
