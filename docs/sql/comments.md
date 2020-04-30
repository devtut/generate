---
metaTitle: "Comments"
description: "Single-line comments, Multi-line comments"
---

# Comments



## Single-line comments


Single line comments are preceded by `--`, and go until the end of the line:

```sql
SELECT *
FROM Employees -- this is a comment
WHERE FName = 'John'

```



## Multi-line comments


Multi-line code comments are wrapped in `/* ... */`:

```sql
/* This query
   returns all employees */
SELECT *
FROM Employees

```

It is also possible to insert such a comment into the middle of a line:

```sql
SELECT /* all columns: */ *
FROM Employees

```

