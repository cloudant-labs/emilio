-module(bad).

-export([a/0, b/1, c/2]).
-export([d/2]).
-export([e/0]).


a() -> ok.


b(A) -> ok.


c(A, B) -> ok.


d(A, B) -> ok.


e() -> ok.
