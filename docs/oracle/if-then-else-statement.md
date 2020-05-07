---
metaTitle: "Oracle Database - IF-THEN-ELSE Statement"
description: "IF-THEN, IF-THEN-ELSE, IF-THEN-ELSIF-ELSE"
---

# IF-THEN-ELSE Statement



## IF-THEN


```sql
DECLARE
v_num1 NUMBER(10);
v_num2 NUMBER(10);

BEGIN
  v_num1 := 2;
  v_num2 := 1;
  
  IF v_num1 > v_num2 THEN
     dbms_output.put_line('v_num1 is bigger than v_num2');
  END IF;
END;

```



## IF-THEN-ELSE


```sql
DECLARE
v_num1 NUMBER(10);
v_num2 NUMBER(10);

BEGIN
  v_num1 := 2;
  v_num2 := 10;
  
  IF v_num1 > v_num2 THEN
     dbms_output.put_line('v_num1 is bigger than v_num2');
  ELSE
    dbms_output.put_line('v_num1 is NOT bigger than v_num2');
  END IF;
END;

```



## IF-THEN-ELSIF-ELSE


```sql
DECLARE
v_num1 NUMBER(10);
v_num2 NUMBER(10);

BEGIN
  v_num1 := 2;
  v_num2 := 2;
  
  IF v_num1 > v_num2 THEN
     dbms_output.put_line('v_num1 is bigger than v_num2');
  ELSIF v_num1 < v_num2 THEN
    dbms_output.put_line('v_num1 is NOT bigger than v_num2');
  ELSE
    dbms_output.put_line('v_num1 is EQUAL to v_num2');
  END IF;
END;

```



#### Syntax


<li>
IF [condition 1] THEN
</li>
<li>
[statements to execute when condition 1 is TRUE];
</li>
<li>
ELSIF [condition 2] THEN
</li>
<li>
[statements to execute when condition 2 is TRUE];
</li>
<li>
ELSE
</li>
<li>
[statements to execute when both condition 1 & condition 2 are FALSE];
</li>
<li>
END IF;
</li>

