Emilio
===

The friendly butler checking your source code.

Implemented Checks
---

* 1XX Indentation Checks
    * 111 indendation is not a multiple of the configured indentation_count
    * 112 indentation increases by more than one or two levels
    * 120 indentation does not match for tokens
    * 121 indentation does not increase for clauses
    * 122 'when' token is not indented two levels
    * 123 indentation does not increase two levels after trailing operator
    * 124 indentation does not increase two levels with prefixed operator
    * 125 indentation for clause body line does not increase by at least 1
    * 126 indentation for expression span line does not increase by at least 1
    * 130 exports not indented correctly

* 2XX White Space Checks
    * 201 white space contains tabs
    * 202 white space contains carriage return
    * 203 line ending has carriage return
    * 204 whitespace contains invalid control or unicode code point
    * 210 file does not end with a single newline
    * 221 space before comma
    * 222 no spaces after comma
    * 223 space before semicolons
    * 224 space before dot
    * 225 space before attribute declaration
    * 230 space after (
    * 231 space before )
    * 240 space after [
    * 241 space before ]
    * 242 no whitespace before |
    * 243 no whitespace after |
    * 250 space after {
    * 251 space before }

* 3XX Empty Line Checks
    * 301 more than two blank lines at module level
    * 302 not two blank lines between functions
    * 310 more than one blank line between function clauses
    * 311 mixed blank line count between function clauses

* 4xx import/export
    * 401 invalid attribute before exports
    * 402 imports used
    * 410 too few export groups
    * 411 too many export groups
    * 412 behavior export groups not ordered correctly
    * 413 behavior callbacks not ordered correctly
    * 420 function order does not match export order
    * 421 private functions mixed with exported functions

* 5XX Physical Line Checks
    * 501 line longer than 80 characters

* 6xx Anti-patterns
    * 601 _Variable reused

* 7xx Logical Analysis
    * 701 case expression has a single clause

* 9XX Internal Errors
    * 901 Unscanable source
    * 902 Unparseable form
    * 903 Internal error
    * 904 Untriggered whitelist entry


Adding a Check
---

To add a new check to emilio you'll need to first implement your check
as a module. The existing `emilio_check_*` modules should provide
decent guidance on the implementation. To include the check you should
add the module name to the list in `emilio.hrl` so that its executed.


Projects For Fuzz Testing
---

[projects.txt](test/projects.txt)
