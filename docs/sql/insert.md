---
metaTitle: "SQL - INSERT"
description: "INSERT data from another table using SELECT, Insert New Row, Insert Only Specified Columns, Insert multiple rows at once"
---

# INSERT




## INSERT data from another table using SELECT


```sql
INSERT INTO Customers (FName, LName, PhoneNumber)
SELECT FName, LName, PhoneNumber FROM Employees

```

This example will insert all [Employees](http://stackoverflow.com/documentation/sql/280/example-database/1014/employees-table#t=201606101401161970524) into the [Customers](http://stackoverflow.com/documentation/sql/280/example-database/1015/customers-table#t=201606101401161970524) table. Since the two tables have different fields and you don't want to move all the fields over, you need to set which fields to insert into and which fields to select. The correlating field names don't need to be called the same thing, but then need to be the same data type. This example is assuming that the Id field has an Identity Specification set and will auto increment.

If you have two tables that have exactly the same field names and just want to move all the records over you can use:

```sql
INSERT INTO Table1
SELECT * FROM Table2

```



## Insert New Row


```sql
INSERT INTO Customers
VALUES ('Zack', 'Smith', 'zack@example.com', '7049989942', 'EMAIL');

```

This statement will insert a new row into the [`Customers`](http://stackoverflow.com/documentation/sql/280/example-database/1015/customers-table#t=201604142311354482065) table. Note that a value was not specified for the `Id` column, as it will be added automatically. However, all other column values must be specified.



## Insert Only Specified Columns


```sql
INSERT INTO Customers (FName, LName, Email, PreferredContact)
VALUES ('Zack', 'Smith', 'zack@example.com', 'EMAIL');

```

This statement will insert a new row into the [`Customers`](http://stackoverflow.com/documentation/sql/280/example-database/1015/customers-table#t=201604142311354482065) table. Data will only be inserted into the columns specified - note that no value was provided for the `PhoneNumber` column. Note, however, that all columns marked as `not null` must be included.



## Insert multiple rows at once


Multiple rows can be inserted with a single insert command:

`INSERT INTO tbl_name (field1, field2, field3)`

`VALUES (1,2,3), (4,5,6), (7,8,9);`

For inserting large quantities of data (bulk insert) at the same time, DBMS-specific features and recommendations exist.

MySQL - [LOAD DATA INFILE](http://dev.mysql.com/doc/refman/5.7/en/load-data.html)

MSSQL - [BULK INSERT](https://msdn.microsoft.com/en-us/library/ms188365.aspx)



#### Syntax


<li>INSERT INTO table_name (column1,column2,column3,...)
VALUES (value1,value2,value3,...);</li>
- INSERT INTO table_name (column1, column2...) SELECT value1, value2... from other_table

