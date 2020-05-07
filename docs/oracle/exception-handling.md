---
metaTitle: "Oracle Database - Exception Handling"
description: "Define custom exception, raise it and see where it comes from, Handling connexion error exceptions"
---

# Exception Handling


Oracle produces a variety of exceptions. You may be surprised how tedious it can be to have your code stop with some unclear message. To improve your PL/SQL code's ability to get fixed easily it is necessary to handle exceptions at the lowest level. Never hide an exception "under the carpet", unless you're here to keep your piece of code for you only and for no one else to maintain.

The [predefined errors](https://docs.oracle.com/database/122/LNPLS/plsql-error-handling.htm#LNPLS00703).



## Define custom exception, raise it and see where it comes from


To illustrate this, here is a function that has 3 different "wrong" behaviors

- parameter is completely stupid: we use a user-defined expression
- parameter has a typo: we use Oracle standard `NO_DATA_FOUND` error
- another, but not handled case

Feel free to adapt it to your standards:

```sql
DECLARE
  this_is_not_acceptable EXCEPTION;
  PRAGMA EXCEPTION_INIT(this_is_not_acceptable, -20077);
  g_err varchar2 (200) := 'to-be-defined';
  w_schema all_tables.OWNER%Type;

  PROCEDURE get_schema( p_table in Varchar2, p_schema out Varchar2)
  Is 
    w_err varchar2 (200) := 'to-be-defined';
  BEGIN
    w_err := 'get_schema-step-1:';
    If (p_table = 'Delivery-Manager-Is-Silly') Then
      raise this_is_not_acceptable;
    end if;
    w_err := 'get_schema-step-2:';
    Select owner Into p_schema 
      From all_tables
     where table_name like(p_table||'%');
  EXCEPTION
  WHEN NO_DATA_FOUND THEN
    -- handle Oracle-defined exception
     dbms_output.put_line('[WARN]'||w_err||'This can happen. Check the table name you entered.');
  WHEN this_is_not_acceptable THEN
    -- handle your custom error
     dbms_output.put_line('[WARN]'||w_err||'Please don''t make fun of the delivery manager.');
  When others then
     dbms_output.put_line('[ERR]'||w_err||'unhandled exception:'||sqlerrm);
     raise;    
  END Get_schema;  

BEGIN
  g_err := 'Global; first call:';
  get_schema('Delivery-Manager-Is-Silly', w_schema);
  g_err := 'Global; second call:';
  get_schema('AAA', w_schema);
  g_err := 'Global; third call:';
  get_schema('', w_schema);
  g_err := 'Global; 4th call:';
  get_schema('Can''t reach this point due to previous error.', w_schema);
  
EXCEPTION
  When others then
     dbms_output.put_line('[ERR]'||g_err||'unhandled exception:'||sqlerrm);
  -- you may raise this again to the caller if error log isn't enough.
--  raise;
END;
/

```

Giving on a regular database:

```sql
[WARN]get_schema-step-1:Please don't make fun of the delivery manager.
[WARN]get_schema-step-2:This can happen. Check the table name you entered.
[ERR]get_schema-step-2:unhandled exception:ORA-01422: exact fetch returns more than requested number of rows
[ERR]Global; third call:unhandled exception:ORA-01422: exact fetch returns more than requested number of rows

```

Remember that exception are here to handle **rare** cases. I saw applications who raised an exception at every access, just to ask for user password, saying "not connected"... so much computation waste.



## Handling connexion error exceptions


Each standard Oracle error is associated with an error number. Its important to anticipate what could go wrong in your code. Here for a connection to another database it can be:

- `-28000` account is locked
- `-28001` password expired
- `-28002` grace period
- `-1017` wrong user / password

Here is a way to test what goes wrong with the user used by the database link:

```sql
declare
  v_dummy number;
begin
  -- testing db link
  execute immediate 'select COUNT(1) from dba_users@pass.world' into v_dummy ;
  -- if we get here, exception wasn't raised: display COUNT's result
  dbms_output.put_line(v_dummy||' users on PASS db');

EXCEPTION
  -- exception can be referred by their name in the predefined Oracle's list
    When LOGIN_DENIED 
    then  
        dbms_output.put_line('ORA-1017 / USERNAME OR PASSWORD INVALID, TRY AGAIN');
    When Others 
    then 
  -- or referred by their number: stored automatically in reserved variable SQLCODE    
        If  SQLCODE = '-2019'
        Then    
          dbms_output.put_line('ORA-2019 / Invalid db_link name');
        Elsif SQLCODE = '-1035'
        Then
          dbms_output.put_line('ORA-1035 / DATABASE IS ON RESTRICTED SESSION, CONTACT YOUR DBA');        
        Elsif SQLCODE = '-28000'
        Then
          dbms_output.put_line('ORA-28000 / ACCOUNT IS LOCKED. CONTACT YOUR DBA');
        Elsif SQLCODE = '-28001'
        Then
          dbms_output.put_line('ORA-28001 / PASSWORD EXPIRED. CONTACT YOUR DBA FOR CHANGE');
        Elsif SQLCODE  = '-28002'
        Then
          dbms_output.put_line('ORA-28002 / PASSWORD IS EXPIRED, CHANGED IT');
        Else
   -- and if it's not one of the exception you expected
          dbms_output.put_line('Exception not specifically handled');
          dbms_output.put_line('Oracle Said'||SQLCODE||':'||SQLERRM);
        End if;
END;
/

```

