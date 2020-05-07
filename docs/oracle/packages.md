---
metaTitle: "Oracle Database - Packages"
description: "Overloading, Define a Package header and body with a function., Package Usage"
---

# Packages




## Overloading


Functions and procedures in packages can be overloaded. The following package **TEST** has two procedures called **print_number**, which behave differently depending on parameters they are called with.

```sql
create or replace package TEST is
  procedure print_number(p_number in integer);
  procedure print_number(p_number in varchar2);
end TEST;
/
create or replace package body TEST is

  procedure print_number(p_number in integer) is
  begin
    dbms_output.put_line('Digit: ' || p_number);    
  end;
  
  procedure print_number(p_number in varchar2) is
  begin
    dbms_output.put_line('String: ' || p_number);    
  end;

end TEST;
/

```

We call both procedures. The first with integer parameter, the second with varchar2.

```sql
set serveroutput on;
-- call the first procedure
exec test.print_number(3);
-- call the second procedure
exec test.print_number('three');

```

The output of the above script is:

```sql
SQL> 
Digit: 3
PL/SQL procedure successfully completed
String: three
PL/SQL procedure successfully completed

```

### Restrictions on Overloading

Only local or packaged subprograms, or type methods, can be overloaded. Therefore, you cannot overload standalone subprograms. Also, you cannot overload two subprograms if their formal parameters differ only in name or parameter mode



## Define a Package header and body with a function.


In this example we define a package header and a package body wit a function. <br>
After that we are calling a function from the package that return a return value. <br>

**Package header**:

```sql
CREATE OR REPLACE PACKAGE SkyPkg AS

       FUNCTION GetSkyColour(vPlanet IN VARCHAR2)
       RETURN VARCHAR2;
       
END;
/

```

**Package body**:

```sql
CREATE OR REPLACE PACKAGE BODY SkyPkg AS

        FUNCTION GetSkyColour(vPlanet IN VARCHAR2)
        RETURN VARCHAR2
        AS
               vColour VARCHAR2(100) := NULL;
        BEGIN
               IF vPlanet = 'Earth' THEN
                       vColour := 'Blue';
               ELSIF vPlanet = 'Mars' THEN
                       vColour := 'Red';
               END IF;

               RETURN vColour;
        END;
        
END;
/

```

**Calling the function from the package body**:

```sql
DECLARE
        vColour VARCHAR2(100);
BEGIN
        vColour := SkyPkg.GetSkyColour(vPlanet => 'Earth');
        DBMS_OUTPUT.PUT_LINE(vColour);
END;
/

```



## Package Usage


Packages in PLSQL are a collection of procedures, functions, variables, exceptions, constants, and data structures.  Generally the resources in a package are related to each other and accomplish similar tasks.

Why Use Packages

- Modularity
- Better Performance/ Funtionality

Parts of a Package

Specification -  Sometimes called a package header.  Contains variable and type declarations and the signatures of the functions and procedures that are in the package which are **public** to be called from outside the package.

Package Body - Contains the code and **private** declarations.

The package specification must be compiled before the package body, otherwise the package body compilation will report an error.



#### Syntax


<li>
CREATE [OR REPLACE] PACKAGE package_name
[AUTHID {CURRENT_USER | DEFINER}]
{IS | AS}
[PRAGMA SERIALLY_REUSABLE;]
[collection_type_definition ...]
[record_type_definition ...]
[subtype_definition ...]
[collection_declaration ...]
[constant_declaration ...]
[exception_declaration ...]
[object_declaration ...]
[record_declaration ...]
[variable_declaration ...]
[cursor_spec ...]
[function_spec ...]
[procedure_spec ...]
[call_spec ...]
[PRAGMA RESTRICT_REFERENCES(assertions) ...]
END [package_name];
</li>
<li>
CREATE OR REPLACE PACKAGE PackageName IS
FUNCTION FunctionName(parameter1 IN VARCHAR2, paramter2 IN NUMBER) RETURN VARCHAR2;
END PackageName;
</li>
<li>
CREATE [OR REPLACE] PACKAGE BODY package_name
{IS | AS}
[PRAGMA SERIALLY_REUSABLE;]
[collection_type_definition ...]
[record_type_definition ...]
[subtype_definition ...]
[collection_declaration ...]
[constant_declaration ...]
[exception_declaration ...]
[object_declaration ...]
[record_declaration ...]
[variable_declaration ...]
[cursor_body ...]
[function_spec ...]
[procedure_spec ...]
[call_spec ...]
END [package_name];
</li>
<li>
CREATE OR REPLACE PACKAGE BODY PackageName IS
FUNCTION FunctionName(parameter1 IN VARCHAR2, paramter2 IN NUMBER) RETURN VARCHAR2 IS
**declarations**
BEGIN
**statements to execute**
RETURN **varchar2 variable**
END FunctionName;
END PackageName;
</li>

