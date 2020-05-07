---
metaTitle: "Oracle Database - Getting started with Oracle Database"
description: "Hello World, SQL Query, Hello world! from table, Hello World from PL/SQL"
---

# Getting started with Oracle Database



## Hello World


```sql
SELECT 'Hello world!' FROM dual;

```

In Oracle's flavor of SQL, ["dual is just a convienence table"](https://asktom.oracle.com/pls/asktom/f?p=100:11:::::P11_QUESTION_ID:1562813956388). It was [originally intended](https://en.wikipedia.org/wiki/DUAL_table#History) to double rows via a JOIN, but now contains one row with a `DUMMY` value of 'X'.



## SQL Query


List employees earning more than $50000 born this century. List their name, date of birth and salary, sorted alphabetically by name.

```sql
SELECT employee_name, date_of_birth, salary
FROM   employees
WHERE  salary > 50000
   AND date_of_birth >= DATE '2000-01-01'
ORDER BY employee_name;

```

Show the number of employees in each department with at least 5 employees. List the largest departments first.

```sql
SELECT department_id, COUNT(*)
FROM   employees
GROUP BY department_id
HAVING COUNT(*) >= 5
ORDER BY COUNT(*) DESC;

```



## Hello world! from table


### Create a simple table

```sql
create table MY_table (
   what varchar2(10), 
   who varchar2(10), 
   mark varchar2(10)
);

```

### Insert values (you can omit target columns if you provide values for all columns)

```sql
insert into my_table (what, who, mark) values ('Hello', 'world', '!' );
insert into my_table values ('Bye bye', 'ponies', '?' );
insert into my_table (what) values('Hey');

```

### Remember to commit, because Oracle uses **transactions**

```sql
commit;

```

### Select your data:

```sql
select what, who, mark from my_table where what='Hello';

```



## Hello World from PL/SQL


```sql
/* PL/SQL is a core Oracle Database technology, allowing you to build clean, secure, 
   optimized APIs to SQL and business logic. */

set serveroutput on 

BEGIN
   DBMS_OUTPUT.PUT_LINE ('Hello World!');
END;

```



#### Remarks


[Oracle](https://en.wikipedia.org/wiki/Oracle_Database) is a relational database management system (RDBMS) originally built by Larry Ellison, Bob Miner, and Ed Oates in the late 70s. It was intended to be compatible with IBM's [System R](https://en.wikipedia.org/wiki/IBM_System_R).

