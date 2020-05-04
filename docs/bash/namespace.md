---
metaTitle: "Namespace"
description: "There are no such things as namespaces"
---

# Namespace



## There are no such things as namespaces


```bash
myfunc(){
    echo "I will never be executed."
}
another_func(){
    # this "redeclare" overwrites original function
    myfunc(){ echo "I am the one and only"; }
}
# myfunc will print "I will never be executed"
myfunc
# but if we call another_func first
another_func
# it gets overwritten and
myfunc
# no prints "I am the one and only"

```

The latest declaration wins. There are no such things as namespaces!
However, functions can contain other functions.

