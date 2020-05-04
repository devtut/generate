---
metaTitle: "Scoping"
description: "Dynamic scoping in action"
---

# Scoping



## Dynamic scoping in action


Dynamic scoping means that variable lookups occur in the scope where a function is **called**, not where it is **defined**.

```bash
$ x=3
$ func1 () { echo "in func1: $x"; }
$ func2 () { local x=9; func1; }
$ func2
in func1: 9
$ func1
in func1: 3

```

In a lexically scoped language, `func1` would **always** look in the global scope for the value of `x`, because `func1` is **defined** in the local scope.

In a dynamically scoped language, `func1` looks in the scope where it is **called**. When it is called from within `func2`, it first looks in the body of `func2` for a value of `x`. If it weren't defined there, it would look in the global scope, where `func2` was called from.

