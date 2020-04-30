---
metaTitle: "Export PostgreSQL database table header and data to CSV file"
description: "Export PostgreSQL table to csv with header for some column(s), Full table backup to csv with header, copy from query"
---

# Export PostgreSQL database table header and data to CSV file


From Adminer management tool it's has export to csv file option for mysql database But not available for postgresql database. Here I will show the command to export CSV for postgresql database.



## Export PostgreSQL table to csv with header for some column(s)


```sql
COPY products(is_public, title, discount) TO 'D:\csv_backup\products_db.csv' DELIMITER ',' CSV HEADER;    

COPY categories(name) TO 'D:\csv_backup\categories_db.csv' DELIMITER ',' CSV HEADER;

```



## Full table backup to csv with header


```sql
COPY products TO 'D:\csv_backup\products_db.csv' DELIMITER ',' CSV HEADER;    

COPY categories TO 'D:\csv_backup\categories_db.csv' DELIMITER ',' CSV HEADER;

```



## copy from query


```sql
copy (select oid,relname from pg_class limit 5) to stdout;

```

