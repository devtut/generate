---
metaTitle: "Oracle Database - constraints"
description: "Update foreign keys with new value in Oracle, Disable all related foreign keys in oracle"
---

# constraints



## Update foreign keys with new value in Oracle


Suppose you have a table and you want to change one of this table primary id. you can use the following scrpit. primary ID here is "PK_S"

```sql
begin
  for i in (select a.table_name, c.column_name
              from user_constraints a, user_cons_columns c
             where a.CONSTRAINT_TYPE = 'R'
               and a.R_CONSTRAINT_NAME = 'PK_S'
               and c.constraint_name = a.constraint_name) loop
  
                     
         execute immediate 'update ' || i.table_name || ' set ' || i.column_name ||
                      '=to_number(''1000'' || ' ||  i.column_name || ') ';              
                    
                     
 
  end loop;

end;

```



## Disable all related foreign keys in oracle


Suppose you have the table T1 and it has relation with many tables and its primary key constraint name is "pk_t1" you want to disable these foreign keys you can use:

```sql
Begin
    For I in (select table_name, constraint_name from user_constraint t where   r_constraint_name='pk_t1') loop

Execute immediate ' alter table ' || I.table_name || ' disable constraint ' || i.constraint_name;

   End loop;
End;﻿

```

