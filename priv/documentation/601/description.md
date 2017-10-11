Do not re-use underscore prefixed variable names

The underscore prefix is used to document unused variables. Reusing an
underscore prefixed variable names leads to subtle bugs when developer
mistakes it for "matches anything". Alternatively, the re-use of
underscore variables for say, printing a debug message is a sign that
a developer has accidentally committed temporary code from
a debugging session.
