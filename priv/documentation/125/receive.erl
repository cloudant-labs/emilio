good() ->
    receive
        Msg ->
            received;
        _Other ->
            not_received
    end.


bad() ->
    receive
        Msg ->
        received;
        _Other ->
        not_received
    end.
