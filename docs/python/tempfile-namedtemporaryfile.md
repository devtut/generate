---
metaTitle: "tempfile NamedTemporaryFile"
description: "Create (and write to a) known, persistant temporary file"
---

# tempfile NamedTemporaryFile




## Create (and write to a) known, persistant temporary file


You can create temporary files which has a visible name on the file system which can be accessed via the `name` property.  The file can, on unix systems, be configured to delete on closure (set by `delete` param, default is True) or can be reopened later.

The following will create and open a named temporary file and write 'Hello World!' to that file.  The filepath of the temporary file can be accessed via `name`, in this example it is saved to the variable `path` and printed for the user.  The file is then re-opened after closing the file and the contents of the tempfile are read and printed for the user.

```py
import tempfile

with tempfile.NamedTemporaryFile(delete=False) as t:
    t.write('Hello World!')
    path = t.name
    print path

with open(path) as t:
    print t.read()

```

Output:

```py
/tmp/tmp6pireJ
Hello World!

```



#### Parameters


|param|description
|------
|mode|mode to open file, default=w+b
|delete|To delete file on closure, default=True
|suffix|filename suffix, default=''
|prefix|filename prefix, default='tmp'
|dir|dirname to place tempfile, default=None
|buffsize|default=-1, (operating system default used)

