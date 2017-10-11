Line endings should not include carriage returns

Carriage returns in line endings generally means developers on non-Unix
platforms have a misconfigured VCS. This leads to spurious whitespace only
hunks in diffs when files are modified by multiple developers on different
platforms.
