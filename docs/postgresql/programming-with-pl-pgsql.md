---
metaTitle: "PostgreSQL - Programming with PL/pgSQL"
description: "Basic PL/pgSQL Function, PL/pgSQL Syntax, RETURNS Block, custom exceptions"
---

# Programming with PL/pgSQL



## Basic PL/pgSQL Function


A simple PL/pgSQL function:

```sql
CREATE FUNCTION active_subscribers() RETURNS bigint AS $$
DECLARE
    -- variable for the following BEGIN ... END block
    subscribers integer;
BEGIN
    -- SELECT must always be used with INTO
    SELECT COUNT(user_id) INTO subscribers FROM users WHERE subscribed;
    -- function result
    RETURN subscribers;
EXCEPTION
    -- return NULL if table "users" does not exist
    WHEN undefined_table
    THEN RETURN NULL;
END;
$$ LANGUAGE plpgsql;

```

This could have been achieved with just the SQL statement but demonstrates the basic structure of a function.

To execute the function do:

```sql
select active_subscribers();

```



## PL/pgSQL Syntax


```sql
CREATE [OR REPLACE] FUNCTION functionName (someParameter 'parameterType') 
RETURNS 'DATATYPE'
AS $_block_name_$
DECLARE
    --declare something
BEGIN
    --do something
    --return something
END;
$_block_name_$
LANGUAGE plpgsql;

```



## RETURNS Block


Options for returning in a PL/pgSQL function:

- `Datatype` [List of all datatypes](https://www.postgresql.org/docs/9.6/static/datatype.html)
- `Table(column_name column_type, ...)`
- `Setof 'Datatype' or 'table_column'`



## custom exceptions


creating custom exception 'P2222':

```sql
create or replace function s164() returns void as
$$
begin
raise exception using message = 'S 164', detail = 'D 164', hint = 'H 164', errcode = 'P2222';
end;
$$ language plpgsql
;

```

creating custom exception not assigning errm:

```sql
create or replace function s165() returns void as
$$
begin
raise exception '%','nothing specified';
end;
$$ language plpgsql
;

```

calling:

```sql
t=# do
$$
declare
 _t text;
begin
  perform s165();
  exception when SQLSTATE 'P0001' then raise info '%','state P0001 caught: '||SQLERRM;
  perform s164();

end;
$$
;
INFO:  state P0001 caught: nothing specified
ERROR:  S 164
DETAIL:  D 164
HINT:  H 164
CONTEXT:  SQL statement "SELECT s164()"
PL/pgSQL function inline_code_block line 7 at PERFORM

```

here custom P0001 processed, and P2222, not, aborting the execution.

Also it makes huge sense to keep a table of exceptions, like here: [http://stackoverflow.com/a/2700312/5315974](http://stackoverflow.com/a/2700312/5315974)



#### Remarks


PL/pgSQL is PostgreSQL's built-in programming language for writing functions which run within the database itself, known as stored procedures in other databases. It extends SQL with loops, conditionals, and return types. Though its syntax may be strange to many developers it is much faster than anything running on the application server because the overhead of connecting to the database is eliminated, which is particularly useful when you would otherwise need to execute a query, wait for the result, and submit another query.

Though many other procedural languages exist for PostgreSQL, such as PL/Python, PL/Perl, and PLV8, PL/pgSQL is a common starting point for developers who want to write their first PostgreSQL function because its syntax builds on SQL. It is also similar to PL/SQL, Oracle's native procedural language, so any developer familiar with PL/SQL will find the language familiar, and any developer who intends to develop Oracle applications in the future but wants to start with a free database can transition from PL/pgSQL to PL/SQL with relative ease.

It should be emphasized that other procedural languages exist and PL/pgSQL is not necessarily superior to them in any way, including speed, but examples in PL/pgSQL can serve as a common reference point for other languages used for writing PostgreSQL functions. PL/pgSQL has the most tutorials and books of all the PLs and can be a springboard to learning the languages with less documentation.

Here are links to some free guides and books on PL/pgSQL:

- The official documentation: [https://www.postgresql.org/docs/current/static/plpgsql.html](https://www.postgresql.org/docs/current/static/plpgsql.html)
- w3resource.com tutorial: [http://www.w3resource.com/PostgreSQL/pl-pgsql-tutorial.php](http://www.w3resource.com/PostgreSQL/pl-pgsql-tutorial.php)
- postgres.cz tutorial: [http://postgres.cz/wiki/PL/pgSQL_(en)](http://postgres.cz/wiki/PL/pgSQL_(en))
- PostgreSQL Server Programming, 2nd Edition: [https://www.packtpub.com/big-data-and-business-intelligence/postgresql-server-programming-second-edition](https://www.packtpub.com/big-data-and-business-intelligence/postgresql-server-programming-second-edition)
- PostgreSQL Developer's Guide: [https://www.packtpub.com/big-data-and-business-intelligence/postgresql-developers-guide](https://www.packtpub.com/big-data-and-business-intelligence/postgresql-developers-guide)

