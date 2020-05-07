---
metaTitle: "Oracle Database - Assignments model and language"
description: "Assignments model in PL/SQL"
---

# Assignments model and language



## Assignments model in PL/SQL


All programming languages allow us to assign values to variables. Usually, a value is assigned to variable, standing on left side. The prototype of the overall assignment operations in any contemporary programming language looks like this:

```sql
left_operand assignment_operand right_operand instructions_of_stop

```

This will assign right operand to the left operand. In PL/SQL this operation looks like this:

```sql
left_operand := right_operand;

```

Left operand **must be always a variable**. Right operand can be value, variable or function:

```sql
set serveroutput on
declare
  v_hello1 varchar2(32767);
  v_hello2 varchar2(32767);
  v_hello3 varchar2(32767);
  function hello return varchar2 is begin return 'Hello from a function!'; end;
begin
   -- from a value (string literal)
  v_hello1 := 'Hello from a value!';
   -- from variable
  v_hello2 := v_hello1;
  -- from function
  v_hello3 := hello; 

  dbms_output.put_line(v_hello1);
  dbms_output.put_line(v_hello2);
  dbms_output.put_line(v_hello3);
end;
/

```

When the code block is executed in SQL*Plus the following output is printed in console:

```sql
Hello from a value!
Hello from a value!
Hello from a function!

```

There is a feature in PL/SQL that allow us to assign "from right to the left". It's possible to do in SELECT INTO statement. Prototype of this instrunction you will find below:

```sql
SELECT [ literal | column_value ]

INTO local_variable

FROM [ table_name | aliastable_name ]

WHERE comparison_instructions;

```

This code will assign character literal to a local variable:

```sql
set serveroutput on
declare
  v_hello varchar2(32767);
begin
  select 'Hello world!'
  into v_hello
  from dual;

  dbms_output.put_line(v_hello);
end;
/

```

When the code block is executed in SQL*Plus the following output is printed in console:

```sql
Hello world!

```

Asignment "from right to the left" **is not a standard**, but it's valuable feature for programmers and users. Generally it's used when programmer is using cursors in PL/SQL - this technique is used, when we want to return a single scalar value or set of columns in the one line of cursor from SQL cursor.

Further Reading:

- [Assigning Values to Variables](http://docs.oracle.com/cd/E11882_01/appdev.112/e25519/fundamentals.htm#LNPLS00205)

