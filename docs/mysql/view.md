---
metaTitle: "VIEW"
description: "Create a View, A view from two tables, Updating a table via a VIEW, DROPPING A VIEW"
---

# VIEW



## Create a View


**Privileges**

The CREATE VIEW statement requires the CREATE VIEW privilege for the view, and some privilege for each column selected by the SELECT statement. For columns used elsewhere in the SELECT statement, you must have the SELECT privilege. If the OR REPLACE clause is present, you must also have the DROP privilege for the view. CREATE VIEW might also require the SUPER privilege, depending on the DEFINER value, as described later in this section.

When a view is referenced, privilege checking occurs.

A view belongs to a database. By default, a new view is created in the default database. To create the view explicitly in a given database, use a fully qualified name

For Example:

db_name.view_name

```sql
mysql> CREATE VIEW test.v AS SELECT * FROM t;

```

**Note - Within a database, base tables and views share the same namespace, so a base table and a view cannot have the same name.**

A VIEW can:

- be created from many kinds of SELECT statements
- refer to base tables or other views
- use joins, UNION, and subqueries
- SELECT need not even refer to any tables

**Another Example**

The following example defines a view that selects two columns from another table as well as an expression calculated from those columns:

```sql
mysql> CREATE TABLE t (qty INT, price INT);
mysql> INSERT INTO t VALUES(3, 50);
mysql> CREATE VIEW v AS SELECT qty, price, qty*price AS value FROM t;
mysql> SELECT * FROM v;
+------+-------+-------+
| qty  | price | value |
+------+-------+-------+
|    3 |    50 |   150 |
+------+-------+-------+

```

**Restrictions**

<li>Before MySQL 5.7.7, the SELECT statement cannot contain a subquery in
the FROM clause.</li>
<li>The SELECT statement cannot refer to system variables or user-defined
variables.</li>
<li>Within a stored program, the SELECT statement cannot refer to program
parameters or local variables.</li>
- The SELECT statement cannot refer to prepared statement parameters.
<li>Any table or view referred to in the definition must exist. After the
view has been created, it is possible to drop a table or view that<br />
the definition refers to. In this case, use of the view results in an
error. To check a view definition for problems of this kind, use the
CHECK TABLE statement.</li>
<li>The definition cannot refer to a TEMPORARY table, and you cannot<br />
create a TEMPORARY view.</li>
- You cannot associate a trigger with a view.
<li>Aliases for column names in the SELECT statement are checked against
the maximum column length of 64 characters (not the maximum alias<br />
length of 256 characters).</li>
<li>A `VIEW` may or may not optimize as well as the equivalent `SELECT`.
It is unlikely to optimize any better.</li>



## A view from two tables


A view is most useful when it can be used to pull in data from more than one table.

```sql
CREATE VIEW myview AS
SELECT a.*, b.extra_data FROM main_table a 
LEFT OUTER JOIN other_table b 
ON a.id = b.id

```

In mysql views are not materialized. If you now perform the simple query `SELECT * FROM myview`, mysql will actually perform the LEFT JOIN behind the scene.

A view once created can be joined to other views or tables



## Updating a table via a VIEW


A `VIEW` acts very much like a table.  Although you can `UPDATE` a table, you may or may not be able to update a view into that table.  In general, if the `SELECT` in the view is complex enough to require a temp table, then `UPDATE` is not allowed.

Things like `GROUP BY`, `UNION`, `HAVING`, `DISTINCT`, and some subqueries prevent the view from being updatable.  Details in [**reference manual**](http://dev.mysql.com/doc/refman/5.7/en/view-updatability.html).



## DROPPING A VIEW


-- Create and drop a view in the current database.

```sql
CREATE VIEW few_rows_from_t1 AS SELECT * FROM t1 LIMIT 10;
DROP VIEW few_rows_from_t1;

```

-- Create and drop a view referencing a table in a different database.

```sql
CREATE VIEW table_from_other_db AS SELECT x FROM db1.foo WHERE x IS NOT NULL;
DROP VIEW table_from_other_db;

```



#### Syntax


<li>
CREATE VIEW view_name AS SELECT column_name(s) FROM table_name WHERE condition; ///Simple create view syntax
</li>
<li>
<p>CREATE
[OR REPLACE]
[ALGORITHM = {UNDEFINED | MERGE | TEMPTABLE}]
[DEFINER = { user | CURRENT_USER }]
[SQL SECURITY { DEFINER | INVOKER }]
VIEW view_name [(column_list)]
AS select_statement
[WITH [CASCADED | LOCAL] CHECK OPTION];  /// Full Create view syntax</p>
</li>
<li>
DROP VIEW [IF EXISTS] [db_name.]view_name; ///Drop view syntax
</li>



#### Parameters


|Parameters|Details
|---|---|---|---
|view_name|Name of View
|SELECT statement|SQL statements to be packed in the views. It can be a SELECT statement to fetch data from one or more tables.



#### Remarks


Views are virtual tables and do not contain the data that is returned. They can save you from writing complex queries again and again.

- **Before a view is made** its specification consists entirely of a `SELECT` statement. The `SELECT` statement cannot contain a sub-query in the FROM clause.
- **Once a view is made** it is used largely just like a table and can be `SELECT`ed from just like a table.

You have to create views, when you want to restrict few columns of your table, from the other user.

- For example: In your organization, you want your managers to view few information from a table named-"Sales", but you don't want that your software engineers can view all fields of table-"Sales". Here, you can create two different views for your managers and your software engineers.

**Performance**.  `VIEWs` are syntactic sugar.  However there performance may or may not be worse than the equivalent query with the view's select folded in.  The Optimizer attempts to do this "fold in" for you, but is not always successful.  MySQL 5.7.6 provides some more enhancements in the Optimizer.  But, regardless, using a `VIEW` will not generate a **faster** query.

