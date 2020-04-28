---
metaTitle: "Checking Path Existence and Permissions"
description: "Perform checks using os.access"
---

# Checking Path Existence and Permissions



## Perform checks using os.access


`os.access` is much better solution to check whether directory exists and it's accesable for reading and writing.

```py
import os
path = "/home/myFiles/directory1"

## Check if path exists
os.access(path, os.F_OK)

## Check if path is Readable
os.access(path, os.R_OK)

## Check if path is Wriable
os.access(path, os.W_OK)

## Check if path is Execuatble
os.access(path, os.E_OK)

```

also it's possible to perfrom all checks together

```py
os.access(path, os.F_OK & os.R_OK & os.W_OK & os.E_OK)

```

All the above returns `True` if access is allowed and `False` if not allowed.
These are available on unix and windows.



#### Parameters


|Parameter|Details
|------
|os.F_OK|Value to pass as the mode parameter of access() to test the existence of path.
|os.R_OK|Value to include in the mode parameter of access() to test the readability of path.
|os.W_OK|Value to include in the mode parameter of access() to test the writability of path.
|os.X_OK|Value to include in the mode parameter of access() to determine if path can be executed.

