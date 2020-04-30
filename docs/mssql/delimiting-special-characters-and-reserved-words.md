---
metaTitle: "Delimiting special characters and reserved words"
description: "Basic Method"
---

# Delimiting special characters and reserved words



## Basic Method


The basic method to escape reserved words for SQL Server is the use of the square brackets (`[` and `]`).  For example, **Description** and **Name** are reserved words; however, if there is an object using both as names, the syntax used is:

```sql
SELECT [Description]
FROM   dbo.TableName
WHERE  [Name] = 'foo'

```

The only special character for SQL Server is the single quote `'` and it is escaped by doubling its usage.  For example, to find the name **O'Shea** in the same table, the following syntax would be used:

```sql
SELECT [Description]
FROM   dbo.TableName
WHERE  [Name] = 'O''Shea'

```



#### Remarks


Generally speaking, it is best not to use [T-SQL Reserved Words](https://msdn.microsoft.com/en-us/library/ms189822.aspx) as table names, column names, programming object names, alias etc. So the method to escape these keywords should only be applied if you are inheriting a database design that cannot be changed.

For reserved words, usage of the square brackets is not mandatory.  When using a tool such as SQL Server Management Studio, the reserved words will be highlighted to bring attention to the fact that they are reserved.

