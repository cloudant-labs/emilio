good(Msg) ->
    receive
        Msg ->
            received;
        _Other ->
            not_received
    end.


bad(Msg) ->
    receive
    Msg ->
        received;
    _Other ->
        not_received
    end.
