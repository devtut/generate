---
metaTitle: "Bash - Case statement"
description: "Simple case statement, Case statement with fall through, Fall through only if subsequent pattern(s) match"
---

# Case statement



## Simple case statement


In its simplest form supported by all versions of bash, case statement executes the case that matches the pattern. `;;` operator breaks after the first match, if any.

```bash
#!/bin/bash

var=1
case $var in
1)
  echo "Antartica"
 ;;
2)
  echo "Brazil"
 ;;
3)
  echo "Cat"
 ;;
esac

```

Outputs:

```bash
Antartica

```



## Case statement with fall through


Since bash 4.0, a new operator `;&` was introduced which provides [fall through](https://en.wikipedia.org/wiki/Switch_statement#Fallthrough) mechanism.

#!/bin/bash

```bash
var=1
case $var in
1)
  echo "Antartica"
  ;&
2)
  echo "Brazil"
  ;&
3)
  echo "Cat"
  ;&
esac

```

Outputs:

```bash
Antartica
Brazil
Cat

```



## Fall through only if subsequent pattern(s) match


Since Bash 4.0, another operator `;;&` was introduced which also provides [fall through](https://en.wikipedia.org/wiki/Switch_statement#Fallthrough) **only if** the patterns in subsequent case statement(s), if any, match.

```bash
#!/bin/bash

var=abc
case $var in
a*)
  echo "Antartica"
  ;;&
xyz)
  echo "Brazil"
  ;;&
*b*)
  echo "Cat"
  ;;&
esac

```

Outputs:

```bash
Antartica
Cat

```

In the below example, the `abc` matches both first and third case but not the second case. So, second case is not executed.

