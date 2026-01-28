---
metaTitle: "Microsoft SQL Server - WHILE loop"
description: "Using While loop, While loop with min aggregate function usage"
---

# WHILE loop



## Using While loop


The `WHILE` loop can be used as an alternative to `CURSORS`. The following example will print numbers from 0 to 99.

```

DECLARE @i int = 0;
 WHILE(@i < 100)
 BEGIN
    PRINT @i;
    SET @i = @i+1
 END

```



## While loop with min aggregate function usage


```sql
DECLARE @ID AS INT;

SET @ID = (SELECT MIN(ID) from TABLE);

WHILE @ID IS NOT NULL
BEGIN
    PRINT @ID;
    SET @ID = (SELECT MIN(ID) FROM TABLE WHERE ID > @ID);
END

```



#### Remarks


Using a `WHILE` loop or other iterative process is not normally the most efficient way to process data in SQL Server.

You should prefer to use a set-based query on the data to achieve the same results, where possible

