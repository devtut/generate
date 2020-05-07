---
metaTitle: "Oracle Database - level query"
description: "Generate N Number of records, Few usages of Level Query"
---

# level query



## Generate N Number of records


```sql
SELECT ROWNUM NO FROM DUAL CONNECT BY LEVEL <= 10

```



## Few usages of Level Query


/* This is a simple query which can generate a sequence of numbers. The following example generates a sequence of numbers from 1..100 */

```sql
select level from dual connect by level <= 100;

```

/*The above query is useful in various scenarios like generating a sequence of dates from a given date. The following query generates 10 consecutive dates */

```sql
select to_date('01-01-2017','mm-dd-yyyy')+level-1 as  dates from dual connect by level <= 10;

```

01-JAN-17<br>
02-JAN-17<br>
03-JAN-17<br>
04-JAN-17<br>
05-JAN-17<br>
06-JAN-17<br>
07-JAN-17<br>
08-JAN-17<br>
09-JAN-17<br>
10-JAN-17<br>



#### Remarks


level clause is responsible for generating N number of dummy records based on some specific condition.

