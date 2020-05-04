---
metaTitle: "Pitfalls"
description: "Whitespace When Assigning Variables, Failed commands do not stop script execution, Missing The Last Line in a File"
---

# Pitfalls



## Whitespace When Assigning Variables


Whitespace matters when assigning variables.

The first two will result in syntax errors (or worse, executing an incorrect command). The last example will correctly set the variable `$foo` to the text "bar".



## Failed commands do not stop script execution


In most scripting languages, if a function call fails, it may throw an exception and stop execution of the program. Bash commands do not have exceptions, but they do have exit codes. A non-zero exit code signals failure, however, a non-zero exit code will not stop execution of the program.

This can lead to dangerous (although admittedly contrived) situations like so:

```bash
#!/bin/bash
cd ~/non/existent/directory
rm -rf *

```

If `cd`-ing to this directory fails, Bash will ignore the failure and move onto the next command, wiping clean the directory from where you ran the script.

The best way to deal with this problem is to make use of the [set](http://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html) command:

```bash
#!/bin/bash
set -e
cd ~/non/existent/directory
rm -rf *

```

`set -e` tells Bash to exit the script immediately if any command returns a non-zero status.



## Missing The Last Line in a File


The C standard says that files should end with a new line, so if EOF comes at the end of a line, that line may not be missed by some commands. As an example:

To make sure this works correctly for in the above example, add a test so that it will continue the loop if the last line is not empty.

