Indentation for `when` clauses should increase two levels

When guards are long enough to require a second line they must be
indented two levels to signal a continuation line. Guards that are
only indented a single level can lead to confusing them with part of
the clause body.
