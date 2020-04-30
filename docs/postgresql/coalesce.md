---
metaTitle: "COALESCE"
description: "Single non null argument, Multiple non null arguments, All null arguments"
---

# COALESCE


Coalesce returns the first none null argument from a set of arguments.
Only the first non null argument is return, all subsequent arguments are ignored.
The function will evaluate to null if all arguments are null.



## Single non null argument


```sql
PGSQL> SELECT COALESCE(NULL, NULL, 'HELLO WORLD');

coalesce
--------
'HELLO WORLD'

```



## Multiple non null arguments


PGSQL> SELECT COALESCE(NULL, NULL, 'first non null', null, null, 'second non null');

```sql
coalesce
--------
'first non null'

```



## All null arguments


```sql
PGSQL> SELECT COALESCE(NULL, NULL, NULL);

coalesce
--------

```

