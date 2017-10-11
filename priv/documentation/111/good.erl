
good_case_expr(Animal) ->
    case Animal of
        kitty ->
            boo;
        doggy ->
            yay;
        _Else ->
            scary
    end.


good_function_call() ->
    some_mod:fun_with_a_long_name(
            And, Lots, Of, Args),
    some_mod:fun_with_a_long_name(
            And,
            Lots,
            Of,
            Args
        ).
