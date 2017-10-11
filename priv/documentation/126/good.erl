
ex_case(Animal) ->
    case {do_some_long_thing(Animal),
            or_something_else_which_is_long(Animal)}
    of
        {bad_kitty, _} ->
            bad_kitty;
        {puppy, awesome} ->
            i_know_right
    end.


ex_try_of(Car) ->
    try {start_engine(Car),
            and_some_other_thing(Car)}
    of
        {started, yay} ->
            started_done
    catch throw:missing_keys ->
        missing_keys
    end.
