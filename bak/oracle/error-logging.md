---
metaTitle: "Oracle Database - Error logging"
description: "Error logging when writing to database"
---

# Error logging



## Error logging when writing to database


Create Oracle error log table ERR$_EXAMPLE for existing EXAMPLE table:

```sql
EXECUTE DBMS_ERRLOG.CREATE_ERROR_LOG('EXAMPLE', NULL, NULL, NULL, TRUE);

```

Make writing operation with SQL:

```sql
insert into EXAMPLE (COL1) values ('example') 
LOG ERRORS INTO ERR$_EXAMPLE reject limit unlimited;

```

