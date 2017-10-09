good(Msg) ->
    receive
        Msg ->
            received;
        _Other ->
            not_received
    after 1000 ->
        got_bored
    end.


bad(Msg) ->
    receive
        Msg ->
            received;
        _Other ->
            not_received
        after 1000 ->
            got_bored
        end.
