---
metaTitle: "Python - sys"
description: "Command line arguments, Script name, Standard error stream, Ending the process prematurely and returning an exit code"
---

# sys


The **sys** module provides access to functions and values concerning the program's runtime environment, such as the command line parameters in `sys.argv` or the function `sys.exit()` to end the current process from any point in the program flow.

While cleanly separated into a module, it's actually built-in and as such will always be available under normal circumstances.



## Command line arguments


```py
if len(sys.argv) != 4:         # The script name needs to be accounted for as well.
    raise RuntimeError("expected 3 command line arguments")

f = open(sys.argv[1], 'rb')    # Use first command line argument.
start_line = int(sys.argv[2])  # All arguments come as strings, so need to be
end_line = int(sys.argv[3])    # converted explicitly if other types are required.

```

Note that in larger and more polished programs you would use modules such as [click](http://click.pocoo.org/) to handle command line arguments instead of doing it yourself.



## Script name


```py
# The name of the executed script is at the beginning of the argv list.
print('usage:', sys.argv[0], '<filename> <start> <end>')

# You can use it to generate the path prefix of the executed program
# (as opposed to the current module) to access files relative to that,
# which would be good for assets of a game, for instance.
program_file = sys.argv[0]

import pathlib
program_path = pathlib.Path(program_file).resolve().parent

```



## Standard error stream


```py
# Error messages should not go to standard output, if possible.
print('ERROR: We have no cheese at all.', file=sys.stderr)

try:
    f = open('nonexistent-file.xyz', 'rb')
except OSError as e:
    print(e, file=sys.stderr)

```



## Ending the process prematurely and returning an exit code


```py
def main():
    if len(sys.argv) != 4 or '--help' in sys.argv[1:]:
        print('usage: my_program <arg1> <arg2> <arg3>', file=sys.stderr)
        
        sys.exit(1)    # use an exit code to signal the program was unsuccessful

    process_data()

```



#### Syntax


<li>
Import the sys module and make it available in the current namespace:

```py
import sys

```


</li>
<li>
Import a specific function from the sys module directly into the current namespace:

```py
from sys import exit

```


</li>



#### Remarks


For details on all **sys** module members, refer to the [official documentation](https://docs.python.org/library/sys.html).

