---
metaTitle: "Oracle Database - JOINS"
description: "CROSS JOIN, LEFT OUTER JOIN, RIGHT OUTER JOIN, FULL OUTER JOIN, ANTIJOIN, INNER JOIN, JOIN, SEMIJOIN, NATURAL JOIN"
---

# JOINS




## CROSS JOIN


A `CROSS JOIN` performs a join between two tables that does not use an explicit join clause and results in the Cartesian product of two tables. A Cartesian product means each row of one table is combined with each row of the second table in the join. For example, if `TABLEA` has 20 rows and `TABLEB` has 20 rows, the result would be `20*20 = 400` output rows.

Example:

```sql
SELECT * 
FROM TABLEA CROSS JOIN TABLEB;

```

This can also be written as:

```sql
SELECT *
FROM TABLEA, TABLEB;

```

Here's an example of cross join in SQL between two tables:

**Sample Table:** TABLEA

```sql
+-------+---------+
| VALUE |   NAME  |
+-------+---------+
|   1   |   ONE   |
|   2   |   TWO   |
+-------+---------+

```

**Sample Table:** TABLEB

```sql
+-------+--------+
| VALUE |  NAME  |
+-------+--------+
|   3   |  THREE |
|   4   |  FOUR  |
+-------+--------+

```

Now, If you execute the query:

```sql
SELECT *
FROM TABLEA CROSS JOIN TABLEB;

```

**Output:**

```sql
+-------+--------+-------+--------+
| VALUE |  NAME  | VALUE |  NAME  |
+-------+--------+-------+--------+
|   1   |   ONE  |   3   |  THREE |
|   1   |   ONE  |   4   |  FOUR  |
|   2   |   TWO  |   3   |  THREE |
|   2   |   TWO  |   4   |  FOUR  |
+-------+--------+-------+--------+

```

This is how cross joining happens between two tables:
[<img src="http://i.stack.imgur.com/fMjRr.jpg" alt="Output" />](http://i.stack.imgur.com/fMjRr.jpg)

More about Cross Join: [Oracle documentation](http://docs.oracle.com/javadb/10.10.1.2/ref/rrefsqljcrossjoin.html)



## LEFT OUTER JOIN


A `LEFT OUTER JOIN` performs a join between two tables that requires an explicit join clause but does not exclude unmatched rows from the first table.

Example:

```sql
SELECT                                      
      ENAME,                                
      DNAME,                                
      EMP.DEPTNO,                           
      DEPT.DEPTNO                           
FROM                                        
      SCOTT.EMP LEFT OUTER JOIN SCOTT.DEPT  
      ON EMP.DEPTNO = DEPT.DEPTNO;          

```

Even though ANSI syntax is the [recommended](https://docs.oracle.com/cd/B28359_01/server.111/b28286/queries006.htm) way, it is likely to encounter legacy syntax very often. Using `(+)` within a condition determines which side of the equation to be considered as **outer**.

```sql
SELECT                                      
      ENAME,                                
      DNAME,                                
      EMP.DEPTNO,                           
      DEPT.DEPTNO                           
FROM                                        
      SCOTT.EMP, 
      SCOTT.DEPT  
WHERE
      EMP.DEPTNO = DEPT.DEPTNO(+);

```

Here's an example of Left Outer Join between two tables:

**Sample Table:** EMPLOYEE

```sql
+-----------+---------+
|    NAME   |  DEPTNO |
+-----------+---------+
|     A     |    2    |
|     B     |    1    |
|     C     |    3    |
|     D     |    2    |
|     E     |    1    |
|     F     |    1    |
|     G     |    4    |
|     H     |    4    |
+-----------+---------+

```

**Sample Table:** DEPT

```sql
+---------+--------------+
|  DEPTNO |   DEPTNAME   |
+---------+--------------+
|    1    |  ACCOUNTING  |
|    2    |    FINANCE   |
|    5    |   MARKETING  |
|    6    |      HR      |
+---------+--------------+

```

Now, If you execute the query:

```sql
SELECT
      *
FROM
      EMPLOYEE LEFT OUTER JOIN DEPT
      ON EMPLOYEE.DEPTNO = DEPT.DEPTNO;

```

**Output:**

```sql
+-----------+---------+---------+--------------+
|    NAME   |  DEPTNO |  DEPTNO |   DEPTNAME   |
+-----------+---------+---------+--------------+
|     F     |    1    |    1    |  ACCOUNTING  |
|     E     |    1    |    1    |  ACCOUNTING  |
|     B     |    1    |    1    |  ACCOUNTING  |
|     D     |    2    |    2    |    FINANCE   |
|     A     |    2    |    2    |    FINANCE   |
|     C     |    3    |         |              |
|     H     |    4    |         |              |
|     G     |    4    |         |              |
+-----------+---------+---------+--------------+

```



## RIGHT OUTER JOIN


A `RIGHT OUTER JOIN` performs a join between two tables that requires an explicit join clause but does not exclude unmatched rows from the second table.

Example:

```sql
SELECT                                      
      ENAME,                                
      DNAME,                                
      EMP.DEPTNO,                           
      DEPT.DEPTNO                           
FROM                                        
      SCOTT.EMP RIGHT OUTER JOIN SCOTT.DEPT  
      ON EMP.DEPTNO = DEPT.DEPTNO;          

```

As the unmatched rows of `SCOTT.DEPT` are included, but unmatched rows of `SCOTT.EMP` are not, the above is equivalent to the following statement using `LEFT OUTER JOIN`.

```sql
SELECT                                      
      ENAME,                                
      DNAME,                                
      EMP.DEPTNO,                           
      DEPT.DEPTNO                           
FROM                                        
      SCOTT.DEPT RIGHT OUTER JOIN SCOTT.EMP  
      ON DEPT.DEPTNO = EMP.DEPTNO;

```

Here's an example of Right Outer Join between two tables:

**Sample Table:** EMPLOYEE

```sql
+-----------+---------+
|    NAME   |  DEPTNO |
+-----------+---------+
|     A     |    2    |
|     B     |    1    |
|     C     |    3    |
|     D     |    2    |
|     E     |    1    |
|     F     |    1    |
|     G     |    4    |
|     H     |    4    |
+-----------+---------+

```

**Sample Table:** DEPT

```sql
+---------+--------------+
|  DEPTNO |   DEPTNAME   |
+---------+--------------+
|    1    |  ACCOUNTING  |
|    2    |    FINANCE   |
|    5    |   MARKETING  |
|    6    |      HR      |
+---------+--------------+

```

Now, If you execute the query:

```sql
SELECT
      *
FROM
      EMPLOYEE RIGHT OUTER JOIN DEPT
      ON EMPLOYEE.DEPTNO = DEPT.DEPTNO;

```

**Output:**

```sql
+-----------+---------+---------+--------------+
|    NAME   |  DEPTNO |  DEPTNO |   DEPTNAME   |
+-----------+---------+---------+--------------+
|     A     |    2    |    2    |    FINANCE   |
|     B     |    1    |    1    |  ACCOUNTING  |
|     D     |    2    |    2    |    FINANCE   |
|     E     |    1    |    1    |  ACCOUNTING  |
|     F     |    1    |    1    |  ACCOUNTING  |
|           |         |    5    |   MARKETING  |
|           |         |    6    |      HR      |
+-----------+---------+---------+--------------+

```

Oracle (+) syntax equivalent for the query is:

```sql
SELECT *
FROM EMPLOYEE, DEPT
WHERE EMPLOYEE.DEPTNO(+) = DEPT.DEPTNO;

```



## FULL OUTER JOIN


A `FULL OUTER JOIN` performs a join between two tables that requires an explicit join clause but does not exclude unmatched rows in either table. In other words, it returns all the rows in each table.

Example:

```sql
SELECT                                     
      *                                    
FROM                                       
      EMPLOYEE FULL OUTER JOIN DEPT 
      ON EMPLOYEE.DEPTNO = DEPT.DEPTNO;         

```

Here's an example of Full Outer Join between two tables:

**Sample Table:** EMPLOYEE

```sql
+-----------+---------+
|    NAME   |  DEPTNO |
+-----------+---------+
|     A     |    2    |
|     B     |    1    |
|     C     |    3    |
|     D     |    2    |
|     E     |    1    |
|     F     |    1    |
|     G     |    4    |
|     H     |    4    |
+-----------+---------+

```

**Sample Table:** DEPT

```sql
+---------+--------------+
|  DEPTNO |   DEPTNAME   |
+---------+--------------+
|    1    |  ACCOUNTING  |
|    2    |    FINANCE   |
|    5    |   MARKETING  |
|    6    |      HR      |
+---------+--------------+

```

Now, If you execute the query:

```sql
SELECT
      *
FROM
      EMPLOYEE FULL OUTER JOIN DEPT
      ON EMPLOYEE.DEPTNO = DEPT.DEPTNO;

```

**Output**

```sql
+-----------+---------+---------+--------------+
|    NAME   |  DEPTNO |  DEPTNO |   DEPTNAME   |
+-----------+---------+---------+--------------+
|     A     |    2    |    2    |    FINANCE   |
|     B     |    1    |    1    |  ACCOUNTING  |
|     C     |    3    |         |              |
|     D     |    2    |    2    |    FINANCE   |
|     E     |    1    |    1    |  ACCOUNTING  |
|     F     |    1    |    1    |  ACCOUNTING  |
|     G     |    4    |         |              |
|     H     |    4    |         |              |
|           |         |    6    |      HR      |
|           |         |    5    |   MARKETING  |
+-----------+---------+---------+--------------+

```

Here the columns that do not match has been kept NULL.



## ANTIJOIN


An antijoin returns rows from the left side of the predicate for which there are no corresponding rows on the right side of the predicate. It returns rows that fail to match (NOT IN) the subquery on the right side.

```sql
SELECT * FROM employees 
   WHERE department_id NOT IN 
   (SELECT department_id FROM departments 
       WHERE location_id = 1700)
   ORDER BY last_name;

```

Here's an example of Anti Join between two tables:

**Sample Table:** EMPLOYEE

```sql
+-----------+---------+
|    NAME   |  DEPTNO |
+-----------+---------+
|     A     |    2    |
|     B     |    1    |
|     C     |    3    |
|     D     |    2    |
|     E     |    1    |
|     F     |    1    |
|     G     |    4    |
|     H     |    4    |
+-----------+---------+

```

**Sample Table:** DEPT

```sql
+---------+--------------+
|  DEPTNO |   DEPTNAME   |
+---------+--------------+
|    1    |  ACCOUNTING  |
|    2    |    FINANCE   |
|    5    |   MARKETING  |
|    6    |      HR      |
+---------+--------------+

```

Now, If you execute the query:

```sql
SELECT
      *
FROM
      EMPLOYEE WHERE DEPTNO NOT IN
      (SELECT DEPTNO FROM DEPT);

```

**Output:**

```sql
+-----------+---------+
|    NAME   |  DEPTNO |
+-----------+---------+
|     C     |    3    |
|     H     |    4    |
|     G     |    4    |
+-----------+---------+

```

The output shows that only the rows of EMPLOYEE table, of which DEPTNO were not present in DEPT table.



## INNER JOIN


An INNER JOIN is a JOIN operation that allows you to specify an explicit join clause.

Syntax

TableExpression [ INNER ] JOIN TableExpression
{
ON booleanExpression |
USING clause
}

You can specify the join clause by specifying ON with a boolean expression.

The scope of expressions in the ON clause includes the current tables and any tables in outer query blocks to the current SELECT. In the following example, the ON clause refers to the current tables:

```sql
-- Join the EMP_ACT and EMPLOYEE tables
-- select all the columns from the EMP_ACT table and 
-- add the employee's surname (LASTNAME) from the EMPLOYEE table
-- to each row of the result
SELECT SAMP.EMP_ACT.*, LASTNAME
 FROM SAMP.EMP_ACT JOIN SAMP.EMPLOYEE
 ON EMP_ACT.EMPNO = EMPLOYEE.EMPNO
-- Join the EMPLOYEE and DEPARTMENT tables, 
-- select the employee number (EMPNO),  
-- employee surname (LASTNAME), 
-- department number (WORKDEPT in the EMPLOYEE table and DEPTNO in the
-- DEPARTMENT table) 
-- and department name (DEPTNAME) 
-- of all employees who were born (BIRTHDATE) earlier than 1930.
SELECT EMPNO, LASTNAME, WORKDEPT, DEPTNAME 
 FROM SAMP.EMPLOYEE JOIN SAMP.DEPARTMENT 
 ON WORKDEPT = DEPTNO 
 AND YEAR(BIRTHDATE) < 1930

-- Another example of "generating" new data values, 
-- using a query which selects from a VALUES clause (which is an 
-- alternate form of a fullselect). 
-- This query shows how a table can be derived called "X"
-- having 2 columns "R1" and "R2" and 1 row of data
SELECT *
FROM (VALUES (3, 4), (1, 5), (2, 6))
AS VALUESTABLE1(C1, C2)
JOIN (VALUES (3, 2), (1, 2),
(0, 3)) AS VALUESTABLE2(c1, c2)
ON VALUESTABLE1.c1 = VALUESTABLE2.c1
-- This results in:
-- C1         |C2         |C1         |2
-- -----------------------------------------------
-- 3          |4          |3          |2
-- 1          |5          |1          |2 


-- List every department with the employee number and 
-- last name of the manager

SELECT DEPTNO, DEPTNAME, EMPNO, LASTNAME
FROM DEPARTMENT INNER JOIN EMPLOYEE
ON MGRNO = EMPNO

-- List every employee number and last name 
-- with the employee number and last name of their manager
SELECT E.EMPNO, E.LASTNAME, M.EMPNO, M.LASTNAME    
FROM EMPLOYEE E INNER JOIN    
DEPARTMENT INNER JOIN EMPLOYEE M 
    ON MGRNO = M.EMPNO
    ON E.WORKDEPT = DEPTNO

```



## JOIN


The `JOIN` operation performs a join between two tables, excluding any unmatched rows from the first table. From Oracle 9i forward, the `JOIN` is equivalent in function to the `INNER JOIN`. This operation requires an explicit join clause, as opposed to the `CROSS JOIN` and `NATURAL JOIN` operators.

Example:

```sql
select t1.*,
       t2.DeptId
  from table_1 t1
  join table_2 t2 on t2.DeptNo = t1.DeptNo

```

Oracle documentation:

- [10g](http://docs.oracle.com/javadb/10.6.2.1/ref/rrefsqlj29840.html#rrefsqlj29840)
- [11g](https://docs.oracle.com/cd/B28359_01/server.111/b28286/queries006.htm)
- [12g](https://docs.oracle.com/database/121/SQLRF/queries006.htm#SQLRF30046)



## SEMIJOIN


A semijoin query can be used, for example, to find all departments with at least one employee whose salary exceeds 2500.

```sql
SELECT * FROM departments 
   WHERE EXISTS 
   (SELECT 1 FROM employees 
       WHERE departments.department_id = employees.department_id 
       AND employees.salary > 2500)
   ORDER BY department_name; 

```

This is more efficient than the full join alternatives, as inner joining on employees then giving a where clause detailing that the salary has to be greater than 2500 could return the same department numerous times. Say if the Fire department has `n` employees all with salary 3000, `select * from departments, employees` with the necessary join on ids and our where clause would return the Fire department `n` times.



## NATURAL JOIN


NATURAL JOIN requires no explitic join condition; it builds one based on all the fields with the same name in the joined tables.

```sql
create table tab1(id number,  descr varchar2(100));
create table tab2(id number,  descr varchar2(100));
insert into tab1 values(1, 'one');
insert into tab1 values(2, 'two');
insert into tab1 values(3, 'three');
insert into tab2 values(1, 'ONE');
insert into tab2 values(3, 'three');

```

The join will be done on the fields ID and DESCR, common to both the tables:

```sql
SQL> select *
  2  from tab1
  3        natural join
  4       tab2;

        ID DESCR
---------- ----------
         3 three

```

Columns with different names will not be used in the JOIN condition:

```sql
SQL> select *
  2  from (select id as id, descr as descr1 from tab1)
  3       natural join
  4       (select id as id, descr as descr2 from tab2);

        ID DESCR1     DESCR2
---------- ---------- ----------
         1 one        ONE
         3 three      three

```

If the joined tables have no common columns, a JOIN with no conditions will be done:

```sql
SQL> select *
  2  from (select id as id1, descr as descr1 from tab1)
  3       natural join
  4       (select id as id2, descr as descr2 from tab2);

       ID1 DESCR1            ID2 DESCR2
---------- ---------- ---------- ----------
         1 one                 1 ONE
         2 two                 1 ONE
         3 three               1 ONE
         1 one                 3 three
         2 two                 3 three
         3 three               3 three

```

