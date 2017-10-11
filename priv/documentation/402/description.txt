Do not use imports

Imports remove the module name association from functions in the
context of invocation. This means that developers reading the code
must memorize the list of imports or constantly check the entire
module for functions. If there is a real need for a shorter module
name use a macro as an alias.
