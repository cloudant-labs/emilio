Do not mix private functions with exported functions

Private functions in a module should be placed below all exported
functions in a module. However, a private function that only differs
in arity and serves as the implementation to a publicly exported
function may be placed directly after a function of the same name. This
is useful when writing a public API that passes default arguments to
the implementation (i.e., passing an empty list as an initial accumulator).
