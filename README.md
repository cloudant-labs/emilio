Emilio
===

The friendly butler checking your source code.

Implemented Checks
---

* 1XX Indentation Checks
    * 111 indendation is not a multiple of four
    * 112 indentation increases by more than one or two levels
    * 120 indentation does not match for tokens
    * 121 indentation does not increase for clauses
    * 122 'when' token is not indented two levels
    * 123 indentation does not increase two levels after trailing operator
    * 124 indentation does not increase two levels with prefixed operator
    * 130 exports not indented correctly

* 2XX White Space Checks
    * 201 white space contains tabs
    * 202 white space contains form feed
    * 203 line ending has form feed
    * 204 whitespace contains invalid control or unicode code point

    * 210 file does not end with a single newline

    * 221 white space before comma
    * 222 no spaces after comma
    * 223 space before semicolons
    * 224 space before dot
    * 225 white space before attribute declaration

    * 230 whitespace after (
    * 231 whitespace before )

    * 240 whitespace after [
    * 241 whitespace before ]
    * 242 no whitespace before |
    * 243 no whitespace after |

    * 250 whitespace after {
    * 251 whitespace before }

* 3XX Empty Line Checks
    * 301 more than two blank lines at module level
    * 302 not two blank lines between functions
    * 310 more than one blank line between function clauses
    * 311 mixed blank line count between function clauses

* 5XX Physical Line Checks
    * 501 line longer than 80 characters

* 9XX Internal Errors
    * 901 Unscanable source
    * 902 Unparseable form
    * 903 Internal error

To Implement
---

* 4xx import/export
    * 401 imports used
    * 410 exports grouped incorrectly
    * 411 second group not gen behavior
    * 420 private functions mixed with exported functions
    * 421 function order does not match export order

* 6xx Anti-patterns
    * 601 _Variable used

* 7xx logical analysis?
    * 701 case statement with single clause?

* 8xx
    * 801 source file missing Apache ASLv2 license


Adding a Check
---

To add a new check to emilio you'll need to first implement your check
as a module. The existing `emilio_check_*` modules should provide
decent guidance on the implementation. To include the check you should
add the module name to the list in `emilio.hrl` so that its executed.


Projects For Fuzz Testing
---

* https://github.com/alpaca-lang/alpaca
* https://github.com/apache/couchdb
* https://github.com/basho/riak
* https://github.com/cloudant/porkrind
* https://github.com/elixir-lang/elixir
* https://github.com/emqtt/emqttd
* https://github.com/erlang/otp
* https://github.com/ferd/recon
* https://github.com/inaka/elvis
* https://github.com/leo-project/leofs
* https://github.com/manopapad/proper
* https://github.com/ninenines/cowboy
* https://github.com/ninenines/ranch
* https://github.com/processone/ejabberd
* https://github.com/rabbitmq/rabbitmq-server
* https://github.com/rvirding/lfe
* https://github.com/synrc/n2o
