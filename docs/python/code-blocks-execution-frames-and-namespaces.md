---
metaTitle: "Code blocks, execution frames, and namespaces"
description: "Code block namespaces"
---

# Code blocks, execution frames, and namespaces


A code block is a piece of Python program text that can be executed as a unit, such as a module, a class definition or a function body. Some code blocks (like modules) are normally executed only once, others (like function bodies) may be executed many times. Code blocks may textually contain other code blocks. Code blocks may invoke other code blocks (that may or may not be textually contained in them) as part of their execution, e.g., by invoking (calling) a function.



## Code block namespaces


|Code Block Type|Global Namespace|Local Namespace
|---|---|---|---|---|---|---|---|---|---
|Module|n.s. for the module|same as global
|Script (file or command)|n.s. for `__main__`|same as global
|Interactive command|n.s. for `__main__`|same as global
|Class definition|global n.s. of containing block|new namespace
|Function body|global n.s. of containing block|new namespace
|String passed to `exec` statement|global n.s. of containing block|local namespace of containing block
|String passed to `eval()`|global n.s. of caller|local n.s. of caller
|File read by `execfile()`|global n.s. of caller|local n.s. of caller
|Expression read by `input()`|global n.s. of caller|local n.s. of caller

