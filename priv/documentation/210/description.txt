Source files should end with a single newline.

Source files that don't end in a single newline don't conform to the POSIX
definition of text files. Source files ending in more than one trailing
newline will commonly introduce spurious whitespace only hunks when the
same file is edited my multiple developers with different editors.
