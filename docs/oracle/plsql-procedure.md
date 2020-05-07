---
metaTitle: "Oracle Database - PLSQL procedure"
description: "Syntax, Hello World, In/Out Parameters"
---

# PLSQL procedure


PLSQL procedure is a group of SQL statements stored on the server for reuse. It increases the performance because the SQL statements do not have to be recompiled every time it is executed.

Stored procedures are useful when same code is required by multiple applications. Having stored procedures eliminates redundancy, and introduces simplicity to the code. When data transfer is required between the client and server, procedures can reduce communication cost in certain situations.



## Syntax


```sql
CREATE [OR REPLACE] PROCEDURE procedure_name
[(parameter_name [IN | OUT | IN OUT] type [, ...])]
{IS | AS}
  < declarations >
BEGIN
  < procedure_body >
EXCEPTION                      -- Exception-handling part begins
  <exception handling goes here >
   WHEN exception1 THEN 
       exception1-handling-statements
END procedure_name;

```


- procedure-name specifies the name of the procedure.
- [OR REPLACE] option allows modifying an existing procedure.
- The optional parameter list contains name, mode and types of the parameters. IN represents that value will be passed from outside and OUT represents that this parameter will be used to return a value outside of the procedure. If no mode is specified, parameter is assumed to be of IN mode.
- In the declaration section we can declare variables which will be used in the body part.
- procedure-body contains the executable part.
- The AS keyword is used instead of the IS keyword for creating a standalone procedure.
- exception section will handle the exceptions from the procedure. This section is optional.



## Hello World


The following simple procedure displays the text "Hello World" in a client that supports [`dbms_output`](https://docs.oracle.com/database/121/ARPLS/d_output.htm).

```sql
CREATE OR REPLACE PROCEDURE helloworld
AS
BEGIN
   dbms_output.put_line('Hello World!');
END;
/

```

You need to execute this at the SQL prompt to create the procedure in the database, or you can run the query below to get the same result:

```sql
SELECT 'Hello World!' from dual;

```



## In/Out Parameters


PL/SQL uses IN, OUT, IN OUT keywords to define what can happen to a passed parameter.

IN specifies that the parameter is read only and the value cannot be changed by the procedure.

OUT specifies the parameter is write only and a procedure can assign a value to it, but not reference the value.

IN OUT specifies the parameter is available for reference and modification.

```sql
PROCEDURE procedureName(x IN INT, strVar IN VARCHAR2, ans OUT VARCHAR2)
...
...
END procedureName;


procedureName(firstvar, secondvar, thirdvar);

```

The variables passed in the above example need to be typed as they are defined in the procedure parameter section.

