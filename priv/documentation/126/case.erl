good(Animal) ->
    case {do_some_long_thing(Animal),
            or_something_else_which_is_long(Animal)}
    of
        {bad_kitty, _} ->
            bad_kitty;
        {puppy, awesome} ->
            i_know_right
    end.


bad(Animal) ->
    case {do_some_long_thing(Animal),
        or_something_else_which_is_long(Animal)} of
        {bad_kitty, _} ->
            bad_kitty;
        {puppy, awesome} ->
            i_know_right
    end.
