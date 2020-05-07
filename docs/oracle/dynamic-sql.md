---
metaTitle: "Oracle Database - Dynamic SQL"
description: "Select value with dynamic SQL, Insert values in dynamic SQL, Update values in dynamic SQL, Execute DDL statement, Execute anonymous block"
---

# Dynamic SQL


Dynamic SQL allows you to assemble an SQL query code in the runtime. This technique has some disadvantages and have to be used very carefully. At the same time, it allows you to implement more complex logic. PL/SQL requires that all objects, used in the code, have to exist and to be valid at compilation time. That's why you can't execute DDL statements in PL/SQL directly, but dynamic SQL allows you to do that.



## Select value with dynamic SQL


Let's say a user wants to select data from different tables. A table is specified by the user.

```

function get_value(p_table_name varchar2, p_id number) return varchar2 is
    value varchar2(100);
  begin
    execute immediate 'select column_value from ' || p_table_name || 
                      ' where id = :P' into value using p_id;
    return value;
  end;

```

Call this function as usual:

```sql
declare
  table_name varchar2(30) := 'my_table';
  id number := 1;
begin
  dbms_output.put_line(get_value(table_name, id));
end;

```

Table to test:

```sql
create table my_table (id number, column_value varchar2(100));
insert into my_table values (1, 'Hello, world!');

```



## Insert values in dynamic SQL


Example below inserts value into the table from the previous example:

```sql
declare
  query_text varchar2(1000) := 'insert into my_table(id, column_value) values (:P_ID, :P_VAL)';
  id number := 2;
  value varchar2(100) := 'Bonjour!';
begin
  execute immediate query_text using id, value;
end;
/

```



## Update values in dynamic SQL


Let's update table from the first example:

```sql
declare
  query_text varchar2(1000) := 'update my_table set column_value = :P_VAL where id = :P_ID';
  id number := 2;
  value varchar2(100) := 'Bonjour le monde!';
begin
  execute immediate query_text using value, id;
end;
/

```



## Execute DDL statement


This code creates the table:

```sql
begin
  execute immediate 'create table my_table (id number, column_value varchar2(100))';
end;
/

```



## Execute anonymous block


You can execute anonymous block. This example shows also how to return value from dynamic SQL:

```sql
declare
  query_text varchar2(1000) := 'begin :P_OUT := cos(:P_IN); end;';
  in_value number := 0;
  out_value number;
begin
  execute immediate query_text using out out_value, in in_value;
  dbms_output.put_line('Result of anonymous block: ' || to_char(out_value));
end;
/

```



#### Remarks


Some important remarks:

<li>
Never use string concatenation to add values to query, use parameters instead. This is wrong:

```sql
execute immediate 'select value from my_table where id = ' || 
     id_valiable into result_variable;

```


And this is right:

```sql
execute immediate 'select value from my_table where id = :P '
    using id_valiable into result_variable;

```


There are two reasons for this. The first is the security. String concatenation allows to make SQL injection. In the query below, if a variable will contain value `1 or 1 = 1`, the `UPDATE` statement will update all lines in the table:

```sql
execute immediate 'update my_table set value = ''I have bad news for you'' where id = ' || id;

```


The second reason is performance. Oracle will parse query without parameters every time when it executes, while query with parameter will be parsed only once in the session.
</li>
<li>
Note, that when the database engine executes a DDL statement, it executes implicit commit before.
</li>

