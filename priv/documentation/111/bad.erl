
bad_case_expr(Animal) ->
   case Animal of
        kitty ->
          boo;
     dog ->
         yay;
      _Else ->
          scary
  end.


bad_function_call() ->
    some_mod:fun_with_a_long_name(
                                  And, Lots, Of, Args
                                  ),
    some_mod:fun_with_a_long_name(
                                  And,
                                  Lots,
                                  Of,
                                  Args).
