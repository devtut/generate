---
metaTitle: "Oracle Database - Triggers"
description: "Before INSERT or UPDATE trigger"
---

# Triggers


**Introduction:**

Triggers are a useful concept in PL/SQL. A trigger is a special type of stored procedure which does not require to be explicitly called by the user. It is a group of instructions, which is automatically fired in response to a specific data modification action on a specific table or relation, or when certain specified conditions are satisfied.
Triggers help maintain the integrity, and security of data. They make the job convenient by taking the required action automatically.



## Before INSERT or UPDATE trigger


```sql
CREATE OR REPLACE TRIGGER CORE_MANUAL_BIUR
  BEFORE INSERT OR UPDATE ON CORE_MANUAL
  FOR EACH ROW
BEGIN
  if inserting then
    -- only set the current date if it is not specified        
    if :new.created is null then
      :new.created := sysdate;
    end if;
  end if;

  -- always set the modified date to now
  if inserting or updating then
    :new.modified := sysdate;
  end if;
end;
/

```



#### Syntax


- CREATE [ OR REPLACE ] TRIGGER trigger_name
- BEFORE UPDATE [or INSERT] [or DELETE]
- ON table_name
- [ FOR EACH ROW ]
- DECLARE
- -- variable declarations
- BEGIN
- -- trigger code
- EXCEPTION
- WHEN ...
- -- exception handling
- END;

