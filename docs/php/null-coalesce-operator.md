---
metaTitle: "Null Coalesce Operator"
description: "General usage"
---

# Null Coalesce Operator


The `null` coalescing operator (**`??`**) has been added as syntactic sugar for the common case of needing to use a ternary in conjunction with `isset()`.

It returns its first operand if it exists and is not `NULL`; otherwise it returns its second operand.



## General usage


```php
// Fetches the value of $_GET['id'] and returns 0 if it does not exist.
$id = $_GET['id'] ?? 0;
// This is equivalent to:
$id = isset($_GET['id']) ? $_GET['id'] : 0;

// Coalescing can be chained: this will return the first defined value out of 
// $_GET['id'], $_POST['id'], and 0.
$id = $_GET['id'] ?? $_POST['id'] ?? 0;

```

