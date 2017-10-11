
ex_list_concat(Arg) ->
    [some_really_long_function(Arg)] ++
            [another_really_long_function(Arg)] ++
                    [yet_another_really_long_function(Arg)].
