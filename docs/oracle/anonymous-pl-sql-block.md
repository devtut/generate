---
metaTitle: "Oracle Database - Anonymous PL/SQL Block"
description: "An example of an anonymous block"
---

# Anonymous PL/SQL Block




## An example of an anonymous block


```sql
DECLARE
    -- declare a variable
    message varchar2(20);
BEGIN
  -- assign value to variable
  message := 'HELLO WORLD';

  -- print message to screen
  DBMS_OUTPUT.PUT_LINE(message);
END;
/

```



#### Remarks


Since they are unnamed, anonymous blocks cannot be referenced by other program units.

