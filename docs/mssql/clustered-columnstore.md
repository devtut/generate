---
metaTitle: "CLUSTERED COLUMNSTORE"
description: "Adding clustered columnstore index on existing table, Table with CLUSTERED COLUMNSTORE index, Rebuild CLUSTERED COLUMNSTORE index"
---

# CLUSTERED COLUMNSTORE



## Adding clustered columnstore index on existing table


CREATE CLUSTERED COLUMNSTORE INDEX enables you to organize a table in column format:

```sql
DROP TABLE IF EXISTS Product
GO
CREATE TABLE Product (
    Name nvarchar(50) NOT NULL,
    Color nvarchar(15),
    Size nvarchar(5) NULL,
    Price money NOT NULL,
    Quantity int
)
GO
CREATE CLUSTERED COLUMNSTORE INDEX cci ON Product 

```



## Table with CLUSTERED COLUMNSTORE index


If you want to have a table organized in column-store format instead of row store, add INDEX cci CLUSTERED COLUMNSTORE in definition of table:

```sql
DROP TABLE IF EXISTS Product
GO
CREATE TABLE Product (
    ProductID int,
    Name nvarchar(50) NOT NULL,
    Color nvarchar(15),
    Size nvarchar(5) NULL,
    Price money NOT NULL,
    Quantity int,
    INDEX cci CLUSTERED COLUMNSTORE
)

```

COLUMSTORE tables are better for tables where you expect full scans and reports, while row store tables are better for tables where you will read or update smaller sets of rows.



## Rebuild CLUSTERED COLUMNSTORE index


Clustered column store index can be rebuilt if you have a lot of deleted rows:

```sql
ALTER INDEX cci ON Products
REBUILD PARTITION = ALL

```

Rebuilding CLUSTERED COLUMNSTORE will "reload" data from the current table into new one and apply compression again, remove deleted rows, etc.

You can rebuild one or more partitions.

