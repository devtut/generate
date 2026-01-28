---
metaTitle: "Oracle Database - DUAL table"
description: "The following example returns the current operating system date and time, The following example generates numbers between  start_value and end_value"
---

# DUAL table



## The following example returns the current operating system date and time


```sql
select sysdate from dual 

```



## The following example generates numbers between  start_value and end_value


```sql
select :start_value + level -1 n
from dual       
connect by level <= :end_value  - :start_value + 1  

```



#### Remarks


`DUAL` table has one column `DUMMY`, defined to be `VARCHAR2(1)` and only one row with a value `x`.

`DUAL` table is automatically created in `SYS` schema when database is created. You can access it from any schema.

You can not change `DUAL` table.

You can use `DUAL` table to call any function from SQL statement. It is useful because it has only one row and oracle optimizer knows everything about it.

