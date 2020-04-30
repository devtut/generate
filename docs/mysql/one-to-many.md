---
metaTitle: "One to Many"
description: "Example Company Tables, Get the Employees Managed by a Single Manager, Get the Manager for a Single Employee"
---

# One to Many


The idea of one to many (1:M) concerns the joining of rows to each other, specifically cases where a single row in one table corresponds to many rows in another.

1:M is one-directional, that is, any time you query a 1:M relationship, you can use the 'one' row to select 'many' rows in another table, but you cannot use a single 'many' row to select more than a single 'one' row.



## Example Company Tables


Consider a company where every employee who is a manager, manages 1 or more employees, and every employee has only 1 manager.

This results in two tables:

**EMPLOYEES**

|EMP_ID|FIRST_NAME|LAST_NAME|MGR_ID
|------
|E01|Johnny|Appleseed|M02
|E02|Erin|Macklemore|M01
|E03|Colby|Paperwork|M03
|E04|Ron|Sonswan|M01

**MANAGERS**

|MGR_ID|FIRST_NAME|LAST_NAME
|------
|M01|Loud|McQueen
|M02|Bossy|Pants
|M03|Barrel|Jones



## Get the Employees Managed by a Single Manager


`SELECT e.emp_id , e.first_name , e.last_name FROM employees e INNER JOIN managers m ON m.mgr_id = e.mgr_id WHERE m.mgr_id = 'M01' ;`

Results in:

|EMP_ID|FIRST_NAME|LAST_NAME
|------
|E02|Erin|Macklemore
|E04|Ron|Sonswan

Ultimately, for every manager we query for, we will see 1 or more employees returned.



## Get the Manager for a Single Employee


**Consult the above example tables when looking at this example.**

`SELECT m.mgr_id , m.first_name , m.last_name FROM managers m INNER JOIN employees e ON e.mgr_id = m.mgr_id WHERE e.emp_id = 'E03' ;`

|MGR_ID|FIRST_NAME|LAST_NAME
|------
|M03|Barrel|Jones

As this is the inverse of the above example, we know that for every employee we query for, we will only ever see one corresponding manager.



#### Remarks


For most cases, working with a 1:M relationship requires us to understand **Primary Keys** and **Foreign Keys**.

**A Primary key** is a column in a table where any single row of that column represents a single entity, or, selecting a value in a primary key column results in exactly one row. Using the above examples, an EMP_ID represents a single employee. If you query for any single EMP_ID, you will see a single row representing the corresponding employee.

**A Foreign Key** is a column in a table that corresponds to the primary key of another different table. From our example above, the MGR_ID in the EMPLOYEES table is a foreign key. Generally to join two tables, you'll join them based on the primary key of one table and the foreign key in another.

