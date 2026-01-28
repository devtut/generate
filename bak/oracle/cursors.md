---
metaTitle: "Oracle Database - Cursors"
description: "Parameterized FOR loop Cursor , Implicit FOR loop cursor, Handling a CURSOR, Working with SYS_REFCURSOR"
---

# Cursors



## Parameterized "FOR loop" Cursor 


```sql
DECLARE
  CURSOR c_emp_to_be_raised(p_sal emp.sal%TYPE) IS 
    SELECT * FROM emp WHERE  sal < p_sal;
BEGIN
  FOR cRowEmp IN c_emp_to_be_raised(1000) LOOP
    dbms_Output.Put_Line(cRowEmp .eName ||' ' ||cRowEmp.sal||'... should be raised ;)');
  END LOOP;
END;
/

```



## Implicit "FOR loop" cursor


```sql
BEGIN
  FOR x IN (SELECT * FROM emp WHERE sal < 100) LOOP
    dbms_Output.Put_Line(x.eName ||' '||x.sal||'... should REALLY be raised :D');
  END LOOP;
END;
/

```


- First advantage is there is no tedious declaration to do (think of this horrible "CURSOR" thing you had in previous versions)
- second advantage is you first build your select query, then when you have what you want, you immediately can access the fields of your query (`x.<myfield>`) in your PL/SQL loop
- The loop opens the cursor and fetches one record at a time for every loop. At the end of the loop the cursor is closed.
- Implicit cursors are faster because the interpreter's work grows as the code gets longer. The less code the less work the interpreter has to do.



## Handling a CURSOR


- Declare the cursor to scan a list of records
- Open it
- Fetch current record into variables (this increments position)
- Use `%notfound` to detect end of list
- Don't forget to close the cursor to limit resources consumption in current context

--

```sql
DECLARE
  CURSOR curCols IS -- select column name and type from a given table
         SELECT column_name, data_type FROM all_tab_columns where table_name='MY_TABLE';
  v_tab_column all_tab_columns.column_name%TYPE;
  v_data_type all_tab_columns.data_type%TYPE;
  v_ INTEGER := 1;
BEGIN
  OPEN curCols;
  LOOP
     FETCH curCols INTO v_tab_column, v_data_type;
     IF curCols%notfound OR v_ > 2000 THEN
       EXIT;
     END IF;
     dbms_output.put_line(v_||':Column '||v_tab_column||' is of '|| v_data_type||' Type.');
     v_:= v_ + 1;
  END LOOP;

  -- Close in any case
  IF curCols%ISOPEN THEN  
     CLOSE curCols;
  END IF;
END;
/

```



## Working with SYS_REFCURSOR


`SYS_REFCURSOR` can be used as a return type when you need to easily handle a list returned not from a table, but more specifically from a function:

### function returning a cursor

```sql
CREATE OR REPLACE FUNCTION list_of (required_type_in IN VARCHAR2)
   RETURN SYS_REFCURSOR
IS
   v_ SYS_REFCURSOR;
BEGIN
   CASE required_type_in
      WHEN 'CATS'
      THEN
         OPEN v_ FOR
           SELECT nickname FROM (
                select 'minou' nickname from dual
      union all select 'minâ'           from dual
      union all select 'minon'          from dual         
           );
      WHEN 'DOGS'
      THEN
         OPEN v_ FOR
              SELECT dog_call FROM (
                select 'bill'   dog_call from dual
      union all select 'nestor'          from dual
      union all select 'raoul'           from dual         
           );
   END CASE;
   -- Whit this use, you must not close the cursor.
   RETURN v_;
END list_of;
/

```

### and how to use it:

```sql
DECLARE
   v_names   SYS_REFCURSOR;
   v_        VARCHAR2 (32767);
BEGIN
   v_names := list_of('CATS');
   LOOP
      FETCH v_names INTO v_;
      EXIT WHEN v_names%NOTFOUND;
      DBMS_OUTPUT.put_line(v_);
   END LOOP;
   -- here you close it
   CLOSE v_names;
END;
/

```



#### Syntax


- Cursor **cursor_name** Is **your_select_statement**
- Cursor **cursor_name**(param TYPE) Is **your_select_statement_using_param**
- FOR x in (**your_select_statement**) LOOP ...



#### Remarks


**Declared Cursors** are difficult to use, and you should prefer `FOR` loops in most cases. What's very interesting in cursors compared to simple `FOR` loops, is that you can parameterize them.

It's better to avoid doing loops with PL/SQL and cursors instead of using Oracle SQL anyway. However, For people accustomed to procedural language, it can be far easier to understand.

If you want to check if a record exists, and then do different things depending on whether the record exists or not, then it makes sense to [use `MERGE` statements](http://stackoverflow.com/documentation/oracle/4193/different-ways-to-update-records/14689/update-using-merge#t=20160803063310475484) in pure ORACLE SQL queries instead of using cursor loops.
(Please note that `MERGE` is only available in Oracle releases >= 9i).

