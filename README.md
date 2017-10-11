Emilio - The Helpful Style Checking Butler for CouchDB
===

Emilio is a fairly opinionated style check for Erlang which
looks to promote consistency, readability, and portability
between developers.

While Emilio attempts to check a large number of common cases
of styles it is by no means exhaustive. Just because Emilio
does not report any errors does not mean a particular source
file has no style issues. Although hopefully the number of checks
that do exist will lead developers to making similar decisions
in the uncovered situations. And as with any given project,
there can always be more checks added in the future.

Implemented Checks
---


| Code | Good | Bad | Description |
| :--- | :--- | :--- | :--- |
| [111](priv/documentation/111/description.md) | [good](priv/documentation/111/good.erl) | [bad](priv/documentation/111/bad.erl) | Indentation should be a multiple of the configured `indentation_count` |
| [112](priv/documentation/112/description.md) | [good](priv/documentation/112/good.erl) | [bad](priv/documentation/112/bad.erl) | Indentation should not increase by more than two levels |
| [120](priv/documentation/120/description.md) | [good](priv/documentation/120/good.erl) | [bad](priv/documentation/120/bad.erl) | Indentation should match for corresponding tokens |
| [121](priv/documentation/121/description.md) | [good](priv/documentation/121/good.erl) | [bad](priv/documentation/121/bad.erl) | Indentation should increase one level for clauses |
| [122](priv/documentation/122/description.md) | [good](priv/documentation/122/good.erl) | [bad](priv/documentation/122/bad.erl) | Indentation for `when` clauses should increase two levels |
| [123](priv/documentation/123/description.md) | [good](priv/documentation/123/good.erl) | [bad](priv/documentation/123/bad.erl) | Indentation for line breaks at operators should increase by two levels |
| [124](priv/documentation/124/description.md) | [good](priv/documentation/124/good.erl) | [bad](priv/documentation/124/bad.erl) | Indentation for line breaks at operators should increase by two levels |
| [125](priv/documentation/125/description.md) | [good](priv/documentation/125/good.erl) | [bad](priv/documentation/125/bad.erl) | Indentation for clause bodies should increase by at least one level |
| [126](priv/documentation/126/description.md) | [good](priv/documentation/126/good.erl) | [bad](priv/documentation/126/bad.erl) | Indentation for expression spans should increase by at least two levels |
| [130](priv/documentation/130/description.md) | [good](priv/documentation/130/good.erl) | [bad](priv/documentation/130/bad.erl) | Exports should be indented one level |
| [201](priv/documentation/201/description.md) | [good](priv/documentation/201/good.erl) | [bad](priv/documentation/201/bad.erl) | Whitespace should not contain tabs |
| [202](priv/documentation/202/description.md) | [good](priv/documentation/202/good.erl) | [bad](priv/documentation/202/bad.erl) | Whitespace should not contain carriage returns |
| [203](priv/documentation/203/description.md) | [good](priv/documentation/203/good.erl) | [bad](priv/documentation/203/bad.erl) | Line endings should not include carriage returns |
| [204](priv/documentation/204/description.md) | [good](priv/documentation/204/good.erl) | [bad](priv/documentation/204/bad.erl) | Whitespace should not contain other non-printable characters |
| [210](priv/documentation/210/description.md) | [good](priv/documentation/210/good.erl) | [bad](priv/documentation/210/bad.erl) | Source files should end with a single newline. |
| [221](priv/documentation/221/description.md) | [good](priv/documentation/221/good.erl) | [bad](priv/documentation/221/bad.erl) | No spaces before commas |
| [222](priv/documentation/222/description.md) | [good](priv/documentation/222/good.erl) | [bad](priv/documentation/222/bad.erl) | Commas should have a trailing space |
| [223](priv/documentation/223/description.md) | [good](priv/documentation/223/good.erl) | [bad](priv/documentation/223/bad.erl) | No spaces before semicolons |
| [224](priv/documentation/224/description.md) | [good](priv/documentation/224/good.erl) | [bad](priv/documentation/224/bad.erl) | No whitespace before dot |
| [225](priv/documentation/225/description.md) | [good](priv/documentation/225/good.erl) | [bad](priv/documentation/225/bad.erl) | Attribute declarations should start in the first column of the line |
| [230](priv/documentation/230/description.md) | [good](priv/documentation/230/good.erl) | [bad](priv/documentation/230/bad.erl) | No spaces after left parenthesis |
| [231](priv/documentation/231/description.md) | [good](priv/documentation/231/good.erl) | [bad](priv/documentation/231/bad.erl) | No spaces before right parenthesis |
| [240](priv/documentation/240/description.md) | [good](priv/documentation/240/good.erl) | [bad](priv/documentation/240/bad.erl) | No spaces after left square brace |
| [241](priv/documentation/241/description.md) | [good](priv/documentation/241/good.erl) | [bad](priv/documentation/241/bad.erl) | No space before right square brace |
| [242](priv/documentation/242/description.md) | [good](priv/documentation/242/good.erl) | [bad](priv/documentation/242/bad.erl) | Pipe operators should have preceding whitespace |
| [243](priv/documentation/243/description.md) | [good](priv/documentation/243/good.erl) | [bad](priv/documentation/243/bad.erl) | Pipe operators should have trailing whitespace |
| [250](priv/documentation/250/description.md) | [good](priv/documentation/250/good.erl) | [bad](priv/documentation/250/bad.erl) | No space after left curly brace |
| [251](priv/documentation/251/description.md) | [good](priv/documentation/251/good.erl) | [bad](priv/documentation/251/bad.erl) | No space before right curly brace |
| [301](priv/documentation/301/description.md) | [good](priv/documentation/301/good.erl) | [bad](priv/documentation/301/bad.erl) | No more than two consecutive blank lines at module level |
| [302](priv/documentation/302/description.md) | [good](priv/documentation/302/good.erl) | [bad](priv/documentation/302/bad.erl) | Functions should be separated by two blank lines |
| [310](priv/documentation/310/description.md) | [good](priv/documentation/310/good.erl) | [bad](priv/documentation/310/bad.erl) | Use zero or one blank lines between function clauses |
| [311](priv/documentation/311/description.md) | [good](priv/documentation/311/good.erl) | [bad](priv/documentation/311/bad.erl) | Use a consistent number of blank lines between function clauses |
| [401](priv/documentation/401/description.md) | [good](priv/documentation/401/good.erl) | [bad](priv/documentation/401/bad.erl) | Only put module and behavior attributes before exports |
| [402](priv/documentation/402/description.md) | [good](priv/documentation/402/good.erl) | [bad](priv/documentation/402/bad.erl) | Do not use imports |
| [410](priv/documentation/410/description.md) | [good](priv/documentation/410/good.erl) | [bad](priv/documentation/410/bad.erl) | Exports should be grouped correctly |
| [411](priv/documentation/411/description.md) | [good](priv/documentation/411/good.erl) | [bad](priv/documentation/411/bad.erl) | Do not use arbitrary groups of exports |
| [412](priv/documentation/412/description.md) | [good](priv/documentation/412/good.erl) | [bad](priv/documentation/412/bad.erl) | Export groups should follow the order of declared behaviors |
| [413](priv/documentation/413/description.md) | [good](priv/documentation/413/good.erl) | [bad](priv/documentation/413/bad.erl) | Order known behavior callbacks consistently |
| [420](priv/documentation/420/description.md) | [good](priv/documentation/420/good.erl) | [bad](priv/documentation/420/bad.erl) | Function definitions should follow the same order as exports |
| [421](priv/documentation/421/description.md) | [good](priv/documentation/421/good.erl) | [bad](priv/documentation/421/bad.erl) | Do not mix private functions with exported functions |
| [501](priv/documentation/501/description.md) | [good](priv/documentation/501/good.erl) | [bad](priv/documentation/501/bad.erl) | Limit source lines to 80 columns |
| [601](priv/documentation/601/description.md) | [good](priv/documentation/601/good.erl) | [bad](priv/documentation/601/bad.erl) | Do not re-use underscore prefixed variable names |
| [701](priv/documentation/701/description.md) | [good](priv/documentation/701/good.erl) | [bad](priv/documentation/701/bad.erl) | Cases should have more than one clause |


Usage
---

```
Usage: ./emilio [-h <help>] [-c <config>] [-i <ignore>] [-j <jobs>]
                [-f <report_formatter>] [-w <whitelist>] path [path ...]

  -h, --help       Show this help message
  -c, --config     The config file to use [default: emilio.cfg]
  -i, --ignore     Ignore any file path matching the specified glob
  -j, --jobs       Number of files to process in parallel [default: 4]
  -f, --format     Set the output format [default: text]
  -w, --whitelist  A CSV file of filename,line,column,code reports to 
                   ignore
  path             Paths to process, directories are searched recursively
```

Adding a Check
---

To add a new check to emilio you'll need to first implement your check
as a module. The existing `emilio_check_*` modules should provide
decent guidance on the implementation. To include the check you should
add the module name to the list in `emilio.hrl` so that its executed.


Exercising Emilio
---

You can exercise Emilio on a number of open source projects
easily by runninng:

    make clone-projects
    make check-projects

The list of projects can be found [here](test/projects.txt).

