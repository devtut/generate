---
metaTitle: "Dynamic SQL"
description: "Execute SQL statement provided as string, Dynamic SQL executed as different user, SQL Injection with dynamic SQL, Dynamic SQL with parameters"
---

# Dynamic SQL



## Execute SQL statement provided as string


In some cases, you would need to execute SQL query placed in string.
EXEC, EXECUTE, or system procedure sp_executesql can execute any SQL query provided as string:

```sql
sp_executesql N'SELECT * FROM sys.objects'
-- or
sp_executesql @stmt = N'SELECT * FROM sys.objects'
-- or
EXEC sp_executesql N'SELECT * FROM sys.objects'
-- or
EXEC('SELECT * FROM sys.columns')
-- or
EXECUTE('SELECT * FROM sys.tables')

```

This procedure will return the same result-set as SQL query provided as statement text.
sp_executesql can execute SQL query provided as string literal, variable/parameter, or even expression:

```sql
declare @table nvarchar(40) = N'product items'
EXEC(N'SELECT * FROM ' + @table)
declare @sql nvarchar(40) = N'SELECT * FROM ' + QUOTENAME(@table);
EXEC sp_executesql @sql

```

You need QUOTENAME function to escape special characters in @table variable. Without this function you would get syntax error if @table variable contains something like spaces, brackets, or any other special character.



## Dynamic SQL executed as different user


You can execute SQL query as different user using AS USER = 'name of database user'

```sql
EXEC(N'SELECT * FROM product') AS USER = 'dbo'

```

SQL query will be executed under dbo database user. All permission checks applicable to dbo user will be checked on SQL query.



## SQL Injection with dynamic SQL


Dynamic queries are

```sql
SET @sql = N'SELECT COUNT(*) FROM AppUsers WHERE Username = ''' + @user + ''' AND Password = ''' + @pass + ''''
EXEC(@sql)

```

If value of user variable is ****myusername'' OR 1=1 --**** the following query will be executed:

```sql
SELECT COUNT(*)
FROM AppUsers 
WHERE Username = 'myusername' OR 1=1 --' AND Password = ''

```

Comment at the end of value of variable @username will comment-out trailing part of the query and condition 1=1 will be evaluated. Application that checks it there at least one user returned by this query will return count greater than 0 and login will succeed.

Using this approach attacker can login into application even if he don't know valid username and password.



## Dynamic SQL with parameters


In order to avoid injection and escaping problems, dynamic SQL queries should be executed with parameters, e.g.:

```sql
SET @sql = N'SELECT COUNT(*) FROM AppUsers WHERE Username = @user AND Password = @pass
EXEC sp_executesql @sql, '@user nvarchar(50), @pass nvarchar(50)', @username, @password

```

Second parameter is a list of parameters used in query with their types, after this list are provided variables that will be used as parameter values.

sp_executesql will escape special characters and execute sql query.

