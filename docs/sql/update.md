---
metaTitle: "UPDATE"
description: "UPDATE with data from another table, Modifying existing values, Updating Specified Rows, Updating All Rows, Capturing Updated records"
---

# UPDATE




## UPDATE with data from another table


The examples below fill in a `PhoneNumber` for any Employee who is also a `Customer` and currently does not have a phone number set in the `Employees` Table.

(These examples use the [Employees](http://stackoverflow.com/documentation/sql/280/example-database/1014/employees-table#t=201606101329266248314) and [Customers](http://stackoverflow.com/documentation/sql/280/example-database/1015/customers-table#t=201606101329266248314) tables from the Example Databases.)

### Standard SQL

Update using a correlated subquery:

```sql
UPDATE 
    Employees
SET PhoneNumber =
    (SELECT 
         c.PhoneNumber
     FROM 
         Customers c
     WHERE 
         c.FName = Employees.FName 
         AND c.LName = Employees.LName)
WHERE Employees.PhoneNumber IS NULL

```

### SQL:2003

Update using `MERGE`:

```sql
MERGE INTO 
    Employees e
USING 
    Customers c 
ON 
    e.FName = c.Fname 
    AND e.LName = c.LName
    AND e.PhoneNumber IS NULL
WHEN MATCHED THEN
   UPDATE 
      SET PhoneNumber = c.PhoneNumber

```

### SQL Server

Update using `INNER JOIN`:

```sql
UPDATE 
    Employees
SET 
    PhoneNumber = c.PhoneNumber
FROM 
    Employees e
INNER JOIN Customers c
        ON e.FName = c.FName 
        AND e.LName = c.LName
WHERE 
    PhoneNumber IS NULL

```



## Modifying existing values


This example uses the [Cars Table](http://stackoverflow.com/documentation/sql/280/example-database/1016/cars-table#t=201606100144025167375) from the Example Databases.

```sql
UPDATE Cars
SET TotalCost = TotalCost + 100
WHERE Id = 3 or Id = 4

```

Update operations can include current values in the updated row. In this simple example the `TotalCost` is incremented by 100 for two rows:

- The TotalCost of Car #3 is increased from 100 to 200
- The TotalCost of Car #4 is increased from 1254 to 1354

A column's new value may be derived from its previous value or from any other column's value in the same table or a joined table.



## Updating Specified Rows


This example uses the [Cars Table](http://stackoverflow.com/documentation/sql/280/example-database/1016/cars-table#t=201606100144025167375) from the Example Databases.

```sql
UPDATE 
    Cars
SET 
    Status = 'READY'
WHERE 
    Id = 4

```

This statement will set the status of the row of 'Cars' with id 4 to "READY".

`WHERE` clause contains a logical expression which is evaluated for each row. If a row fulfills the criteria, its value is updated. Otherwise, a row remains unchanged.



## Updating All Rows


This example uses the [Cars Table](http://stackoverflow.com/documentation/sql/280/example-database/1016/cars-table#t=201606100144025167375) from the Example Databases.

```sql
UPDATE Cars
SET Status = 'READY'

```

This statement will set the 'status' column of all rows of the 'Cars' table to "READY" because it does not have a `WHERE` clause to filter the set of rows.



## Capturing Updated records


Sometimes one wants to capture the records that have just been updated.

```sql
CREATE TABLE #TempUpdated(ID INT)

Update TableName SET Col1 = 42
    OUTPUT inserted.ID INTO #TempUpdated
    WHERE Id > 50

```



#### Syntax


<li>UPDATE **table**<br />
SET **column_name** = **value**, **column_name2** = **value_2**, ..., **column_name_n** = **value_n**<br />
WHERE **condition** (**logical operator** condition_n)</li>

