---
metaTitle: "PostgreSQL - Postgres Tip and Tricks"
description: "DATEADD alternative in Postgres, Comma seperated values of a column, Delete duplicate records from postgres table, Update query with join between two tables alternative since Postresql does not support join in update query., Difference between two date timestamps month wise and year wise, Query to Copy/Move/Transafer table data from one database to other database table with same schema"
---

# Postgres Tip and Tricks




## DATEADD alternative in Postgres


- `SELECT CURRENT_DATE + '1 day'::INTERVAL`
- `SELECT '1999-12-11'::TIMESTAMP + '19 days'::INTERVAL`
- `SELECT '1 month'::INTERVAL + '1 month 3 days'::INTERVAL`



## Comma seperated values of a column


```sql
SELECT 
    string_agg(<TABLE_NAME>.<COLUMN_NAME>, ',') 
FROM 
    <SCHEMA_NAME>.<TABLE_NAME> T

```



## Delete duplicate records from postgres table


```sql
DELETE 
    FROM <SCHEMA_NAME>.<Table_NAME> 
WHERE 
    ctid NOT IN
        (
        SELECT 
            MAX(ctid) 
        FROM 
            <SCHEMA_NAME>.<TABLE_NAME> 
        GROUP BY 
            <SCHEMA_NAME>.<TABLE_NAME>.*
        )
;

```



## Update query with join between two tables alternative since Postresql does not support join in update query.


```

   update <SCHEMA_NAME>.<TABLE_NAME_1> AS A
    SET <COLUMN_1> = True       
    FROM <SCHEMA_NAME>.<TABLE_NAME_2> AS B 
    WHERE 
        A.<COLUMN_2> = B.<COLUMN_2> AND
        A.<COLUMN_3> = B.<COLUMN_3>

```



## Difference between two date timestamps month wise and year wise


Monthwise difference between two dates(timestamp)

```sql
select 
    (
        (DATE_PART('year', AgeonDate) - DATE_PART('year', tmpdate)) * 12 
        +
        (DATE_PART('month', AgeonDate) - DATE_PART('month', tmpdate))
    ) 
from dbo."Table1"

```

Yearwise difference between two dates(timestamp)

```sql
select (DATE_PART('year', AgeonDate) - DATE_PART('year', tmpdate)) from dbo."Table1"

```



## Query to Copy/Move/Transafer table data from one database to other database table with same schema


First Execute

```sql
CREATE EXTENSION DBLINK;

```

Then

```sql
INSERT INTO 
    <SCHEMA_NAME>.<TABLE_NAME_1> 
SELECT * 
FROM 
    DBLINK(
    'HOST=<IP-ADDRESS> USER=<USERNAME> PASSWORD=<PASSWORD> DBNAME=<DATABASE>',
    'SELECT * FROM <SCHEMA_NAME>.<TABLE_NAME_2>')
    AS <TABLE_NAME>
    (
    <COLUMN_1> <DATATYPE_1>, 
    <COLUMN_1> <DATATYPE_2>, 
    <COLUMN_1> <DATATYPE_3>
    );

```

