---
metaTitle: "Oracle Database - Bulk collect"
description: "Bulk data Processing"
---

# Bulk collect



## Bulk data Processing


local collections are not allowed in select statements. Hence the first step is to create a schema level collection.
If the collection is not schema level and being used in SELECT statements then it would cause "PLS-00642: local collection types not allowed in SQL statements"

```sql
CREATE OR REPLACE TYPE table1_t IS OBJECT (
a_1 INTEGER,
a_2 VARCHAR2(10)
);

```

--Grant permissions on collection so that it could be used publically in database

```

    GRANT EXECUTE ON table1_t TO PUBLIC;
     CREATE OR REPLACE TYPE table1_tbl_typ IS TABLE OF table1_t;
     GRANT EXECUTE ON table1_tbl_typ TO PUBLIC;

```

--fetching data from table into collection and then loop through the collection and print the data.

```

   DECLARE
     table1_tbl table1_tbl_typ;
    BEGIN
     table1_tbl := table1_tbl_typ();
      SELECT table1_t(a_1,a_2)  
      BULK COLLECT INTO table1_tbl 
      FROM table1 WHERE ROWNUM<10;

     FOR rec IN (SELECT a_1 FROM TABLE(table1_tbl))--table(table1_tbl) won't give error)
     LOOP
       dbms_output.put_line('a_1'||rec.a_1);
       dbms_output.put_line('a_2'||rec.a_2);
     END LOOP;
    END;
/

```

