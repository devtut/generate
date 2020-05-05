---
metaTitle: "MySQL - PREPARE Statements"
description: "PREPARE, EXECUTE and DEALLOCATE PREPARE Statements, Alter table with add column, Construct and execute"
---

# PREPARE Statements



## PREPARE, EXECUTE and DEALLOCATE PREPARE Statements


[PREPARE](http://dev.mysql.com/doc/refman/5.7/en/prepare.html) prepares a statement for execution

[EXECUTE](http://dev.mysql.com/doc/refman/5.7/en/execute.html) executes a prepared statement

[DEALLOCATE PREPARE](http://dev.mysql.com/doc/refman/5.7/en/deallocate-prepare.html) releases a prepared statement

```sql
SET @s = 'SELECT SQRT(POW(?,2) + POW(?,2)) AS hypotenuse';
PREPARE stmt2 FROM @s;
SET @a = 6;
SET @b = 8;
EXECUTE stmt2 USING @a, @b;

```

Result:

```sql
+------------+
| hypotenuse |
+------------+
|         10 |
+------------+

```

Finally,

```sql
DEALLOCATE PREPARE stmt2;

```

Notes:

- You must use @variables, not DECLAREd variables for `FROM @s`
- A primary use for Prepare, etc, is to 'construct' a query for situations where binding will not work, such as inserting the table name.



## Alter table with add column


```sql
SET v_column_definition := CONCAT(
  v_column_name
  ,' ',v_column_type
  ,' ',v_column_options 
);

SET @stmt := CONCAT('ALTER TABLE ADD COLUMN ', v_column_definition);

PREPARE stmt FROM @stmt;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

```



## Construct and execute


(This is a request for a good example that shows how to **construct** a `SELECT` using `CONCAT`, then prepare+execute it.  Please emphasize the use of @variables versus DECLAREd variables -- it makes a big difference, and it is something that novices (include myself) stumble over.)



#### Syntax


- PREPARE stmt_name FROM preparable_stmt
- EXECUTE stmt_name [USING @var_name [, @var_name] ...]
- {DEALLOCATE | DROP} PREPARE stmt_name

