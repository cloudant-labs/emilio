
-module(bad).
-behavior(naughty).

-include_lib("path/to/something.hrl").

-define(THIS_DOES_NOT_BELONG_HERE, because_silly).

-export([
    bad_module/0
]).


bad_module() ->
    gets_the_water_bottle.
