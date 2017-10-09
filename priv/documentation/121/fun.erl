good() ->
    fun
        (Arg) when is_atom(Arg) ->
            Arg;
        (Arg) when is_list(Arg) ->
            list_to_existing_atom(Arg)
    end.


bad() ->
    fun(Arg) when is_atom(Arg) ->
        Arg;
    (Arg) when is_list(Arg) ->
        list_to_existing_atom(Arg)
    end.
