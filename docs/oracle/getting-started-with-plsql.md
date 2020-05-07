---
metaTitle: "Oracle Database - Getting started with plsql"
description: "Hello World, Definition of PLSQL, Difference between %TYPE and %ROWTYPE., About PLSQL, Create or replace a view, Create a table"
---

# Getting started with plsql



## Hello World


```sql
set serveroutput on

DECLARE
   message constant varchar2(32767):= 'Hello, World!';
BEGIN
   dbms_output.put_line(message);
END;
/

```

Command `set serveroutput on` is required in SQL*Plus and SQL Developer clients to enable the output of `dbms_output`. Without the command nothing is displayed.

The `end;` line signals the end of the anonymous PL/SQL block. To run the code from SQL command line, you may need to type `/` at the beginning of the first blank line after the last line of the code. When the above code is executed at SQL prompt, it produces the following result:

```sql
Hello, World!

PL/SQL procedure successfully completed.

```



## Definition of PLSQL


PL/SQL (Procedural Language/Structured Query Language) is Oracle Corporation's procedural extension for SQL and the Oracle relational database. PL/SQL is available in Oracle Database (since version 7), TimesTen in-memory database (since version 11.2.1), and IBM DB2 (since version 9.7).

The basic unit in PL/SQL is called a block, which is made up of three parts: a declarative part, an executable part, and an exception-building part.

```sql
DECLARE
   <declarations section>
BEGIN
   <executable command(s)>
EXCEPTION
   <exception handling>
END;

```

**Declarations** - This section starts with the keyword DECLARE. It is an optional   section and defines all variables, cursors, subprograms, and other elements to be used in the program.

**Executable Commands** - This section is enclosed between the keywords BEGIN and END and it is a mandatory section. It consists of the executable PL/SQL statements of the program. It should have at least one executable line of code, which may be just a NULL command to indicate that nothing should be executed.

**Exception Handling** - This section starts with the keyword EXCEPTION. This section is again optional and contains exception(s) that handle errors in the program.

Every PL/SQL statement ends with a semicolon (;). PL/SQL blocks can be nested within other PL/SQL blocks using BEGIN and END.

In anonymous block, only executable part of block is required, other parts are not nessesary. Below is example of simple anonymous code, which does not do anything but perform without error reporting.

```sql
BEGIN
    NULL;
END;
/ 

```

Missing excecutable instruction leads to an error, becouse PL/SQL does not support empty blocks. For example, excecution of code below leads to an error:

```sql
BEGIN
END;
/ 

```

Application will raise error:

```sql
END;
*
ERROR at line 2:
ORA-06550: line 2, column 1:
PLS-00103: Encountered the symbol "END" when expecting one of the following:
( begin case declare exit for goto if loop mod null pragma
raise return select update while with <an identifier>
<a double-quoted delimited-identifier> <a bind variable> <<
continue close current delete fetch lock insert open rollback
savepoint set sql execute commit forall merge pipe purge

```

Symbol " * " in line below keyword "END;" means, that the block which ends with this block is empty or bad constructed. Every execution block needs instructions to do, even if it does nothing, like in our example.



## Difference between %TYPE and %ROWTYPE.


`%TYPE`: Used to declare a field with the same type as that of a specified table's column. <br>

```sql
DECLARE
        vEmployeeName   Employee.Name%TYPE;
BEGIN
        SELECT Name 
        INTO   vEmployeeName
        FROM   Employee
        WHERE  RowNum = 1;
        
        DBMS_OUTPUT.PUT_LINE(vEmployeeName);
END;
/

```

%ROWTYPE: Used to declare a record with the same types as found in the specified table, view or cursor (= multiple columns). <br>

```sql
DECLARE
        rEmployee     Employee%ROWTYPE;
BEGIN
        rEmployee.Name := 'Matt';
        rEmployee.Age := 31;
        
        DBMS_OUTPUT.PUT_LINE(rEmployee.Name);
        DBMS_OUTPUT.PUT_LINE(rEmployee.Age);
END;
/

```



## About PLSQL


PL/SQL stands for Procedural Language extensions to SQL. PL/SQL is available only as an "enabling technology" within other software products; it does not exist as a standalone language. You can use PL/SQL in the Oracle relational database, in the Oracle Server, and in client-side application development tools, such as Oracle Forms. Here are some of the ways you might use PL/SQL:

1. To build stored procedures.     .
1. To create database triggers.
1. To implement client-side logic in your Oracle Forms application.
1. To link a World Wide Web home page to an Oracle database.



## Create or replace a view


In this example we are going to create a view. <br>
A view is mostly used as a simple way of fetching data from multiple tables. <br>

Example 1: <br>
View with a select on one table.

```sql
CREATE OR REPLACE VIEW LessonView AS
       SELECT     L.*
       FROM       Lesson L;

```

Example 2: <br>
View with a select on multiple tables.

```sql
CREATE OR REPLACE VIEW ClassRoomLessonView AS
       SELECT     C.Id, 
                  C.Name, 
                  L.Subject, 
                  L.Teacher 
       FROM       ClassRoom C, 
                  Lesson L 
       WHERE      C.Id = L.ClassRoomId;

```

To call this views in a query you can use a select statement. <br>

```sql
SELECT * FROM LessonView;
SELECT * FROM ClassRoomLessonView;

```



## Create a table


Below we are going to create a table with 3 columns. <br>
The column `Id` must be filled is, so we define it `NOT NULL`. <br>
On the column `Contract` we also add a check so that the only value allowed is 'Y' or 'N'. If an insert in done and this column is not specified during the insert then default a 'N' is inserted.

```sql
CREATE TABLE Employee (
        Id            NUMBER NOT NULL,
        Name          VARCHAR2(60),
        Contract      CHAR DEFAULT 'N' NOT NULL,
        ---
        CONSTRAINT p_Id PRIMARY KEY(Id),
        CONSTRAINT c_Contract CHECK (Contract IN('Y','N'))
);

```



#### Remarks


This section provides an overview of what plsql is, and why a developer might want to use it.

It should also mention any large subjects within plsql, and link out to the related topics.  Since the Documentation for plsql is new, you may need to create initial versions of those related topics.

