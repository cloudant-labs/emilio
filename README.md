Emilio
===

The friendly butler checking your source code.

Implemented Checks
---

* 111 indendation is not a multiple of four
* 112 indentation increases by more than one or two levels
* 120 indentation does not match for tokens
* 121 indentation does not increase for clauses
* 122 'when' token is not indented two levels

* 201 white space contains tabs
* 202 white space contains form feed
* 203 line ending has form feed
* 204 whitespace contains invalid control or unicode code point

* 210 file does not end with a single newline

* 222 spaces after commas

* 501 line longer than 80 characters

* 901 Unscanable source
* 902 Unparseable form

To Implement
---

* 123 indentation does not increase two levels after trailing operator
* 124 indentation does not increase two levels with prefixed operator
* 130 exports not indented correctly

* 221 spaces around operators
* 230 no spaces before commas
* 231 no spaces before semicolons
* 232 no spaces before dot
* 234 no spaces after `{[(`
* 235 no spaces before `)]}`

* 3xx blank lines
  * 301 two blank lines between different attributes
  * 302 one blank line between same attribute
  * 303 one or zero blank lines between export functions
  * 310 two blank lines between functions
  * 311 zero or one blank line between function clauses
  * 312 same number of blank lines between function clauses

* 4xx import/export
  * 401 No imports used
  * 410 exports grouped correctly
  * 411 gen behaviors in second group
  * 420 no private functions mixed with exported functions
  * 421 function order matches export order

* 6xx
  * 601 source file missing Apache ASLv2 license

* 7xx logical analysis?
  * 701 case statement with single clause?


Notes
---

* 901 - Emilio uses a custom parser so that it doesn't have to expand
        macros. This means that there are cases when a macro that rewrites
        source forms in a way that emilio is unable to parse the unexpanded
        source code. For now this is acceptable as it doesn't happen
        that often but it may be something to figure out later.


Adding a Check
---

To add a new check to emilio you'll need to first implement your check
as a module. The existing `emilio_check_*` modules should provide
decent guidance on the implementation. To include the check you should
add the module name to the list in `emilio.hrl` so that its executed.
