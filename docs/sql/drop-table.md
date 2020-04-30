---
metaTitle: "DROP Table"
description: "Check for existence before dropping, Simple drop"
---

# DROP Table




## Check for existence before dropping


```sql
DROP TABLE IF EXISTS MyTable;

```

```sql
DROP TABLE IF EXISTS MyTable;

```

```sql
If Exists(Select * From Information_Schema.Tables
          Where Table_Schema = 'dbo'
            And Table_Name = 'MyTable')
  Drop Table dbo.MyTable

```

```sql
DROP TABLE IF EXISTS MyTable;

```



## Simple drop


```sql
Drop Table MyTable;

```



#### Remarks


DROP TABLE removes the table definition from the schema along with the rows, indexes, permissions, and triggers.

