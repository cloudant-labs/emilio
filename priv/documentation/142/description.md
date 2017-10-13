Inline funs as arguments should end on the last line of the call

There are multiple approaches on formatting inline funs. Its important to
be consistent across a code base so this check exists to enforce the more
common approach of requiring the fun on the same line as the function call
its being passed to. If this causes line length issues or other formatting
oddities then the fun should be assigned to a temporary variable that's
passed as an argument instead.
