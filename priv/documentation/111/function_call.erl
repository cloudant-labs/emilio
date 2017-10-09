good() ->
    some_mod:fun_with_a_long_name(
            And, Lots, Of, Args),
    some_mod:fun_with_a_long_name(
            And,
            Lots,
            Of,
            Args
        ).


bad() ->
    some_mod:fun_with_a_long_name(
                                  And, Lots, Of, Args
                                 ),
    some_mod:fun_with_a_long_name(
                                  And,
                                  Lots,
                                  Of,
                                  Args).
