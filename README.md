Emilio
===

The friendly butler checking your source code.

Checks
---

* 111 indendation is not a multiple of four

* 191 white space contains tabs
* 192 white space contains form feed
* 193 line ending has form feed
* 194 whitespace contains invalid control or unicode code point

* 501 Line longer than 80 characters


Groups of errors and warnings:
E errors
W warnings
100 indentation
200 whitespace
300 blank lines
400 imports
500 line length
600 deprecation
700 statements
900 syntax error



E112 expected an indented block
E113 unexpected indentation
E114 (comment) indentation is not a multiple of four
E115 (comment) expected an indented block
E116 (comment) unexpected indentation

E121 under-indented for hanging indent
E122 missing indentation or outdented
E123 closing bracket does not match indentation of opening bracket's line
E124 closing bracket does not match visual indentation
E125 continuation line with same indent as next logical line
E126 over-indented for hanging indent
E127 over-indented for visual indent
E128 continuation line under-indented for visual indent
E129 visually indented line with same indent as next logical line

E131 unaligned for hanging indent
E133 closing bracket is missing indentation


E201 whitespace after '%s'
E202 whitespace before '}])'
E203 whitespace before ',;:'

E211 whitespace before '(['

E221 multiple spaces before operator
E222 multiple spaces after operator
E223 tab before operator
E224 tab after operator
E225 missing whitespace around operator
E226 missing whitespace around arithmetic operator
E227 missing whitespace around bitwise or shift operator
E228 missing whitespace around modulo operator


E231 missing whitespace after comma, semicolon, or colon

E241 multiple spaces after comma
E242 tab after comma

E251 unexpected spaces around keyword / parameter equals

E261 at least two spaces before inline comment
E262 inline comment should start with '# '
E265 block comment should start with '# '
E266 too many leading '#' for block comment

E271 multiple spaces after keyword
E272 multiple spaces before keyword
E273 tab after keyword
E274 tab before keyword
E275 missing whitespace after import keyword

W291 Trailing whitespace
W292 Missing newline at end of file
W293 Blank line contains whitespace
294 No form feeds in source files

E301 expected 1 blank line, found 0
E302 expected 2 blank lines, found %d
E303 too many blank lines (%d)
E304 blank lines found after function decorator
E305 expected 2 blank lines after class or function definition, found %d
E306 expected 1 blank line before a nested definition, found 0
W391 Trailing blank lines

E401 multiple imports on one line
E402 module level import not at top of file


E502 the backslash is redundant between brackets
W503 line break before binary operator

E701 multiple statements on one line (colon)
E702 multiple statements on one line (semicolon)
E703 statement ends with a semicolon

E711 logical comparison to None not using is/is not
E712 logical comparison to True/False not using is/is not
E713 test for membership should be 'not in'
E714 test for object identity should be 'is not'

E721 do not compare types, use 'isinstance()'
E722 do not use bare except'

E731 do not assign a lambda expression, use a def

E741 ambiguous variable name l, O, or I
E742 ambiguous class definition l, O, or I
E743 ambiguous function definition l, O, or I
