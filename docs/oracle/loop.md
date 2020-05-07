---
metaTitle: "Oracle Database - Loop"
description: "Simple Loop, WHILE Loop, FOR Loop"
---

# Loop



## Simple Loop


```sql
DECLARE
v_counter NUMBER(2);

BEGIN
  v_counter := 0;
  LOOP
    v_counter := v_counter + 1;
    dbms_output.put_line('Line number' || v_counter);
    
    EXIT WHEN v_counter = 10;
  END LOOP;
END;

```



## WHILE Loop


The WHILE loop is executed untill the condition of end is fulfilled. Simple example:

```sql
DECLARE
v_counter NUMBER(2); --declaration of counter variable

BEGIN
  v_counter := 0; --point of start, first value of our iteration
 
  WHILE v_counter < 10 LOOP --exit condition
    
    dbms_output.put_line('Current iteration of loop is ' || v_counter); --show current iteration number in dbms script output
    v_counter := v_counter + 1; --incrementation of counter value, very important step

  END LOOP; --end of loop declaration
END;

```

This loop will be executed untill current value of variable v_counter will be less than ten.

The result:

```sql
Current iteration of loop is 0
Current iteration of loop is 1
Current iteration of loop is 2
Current iteration of loop is 3
Current iteration of loop is 4
Current iteration of loop is 5
Current iteration of loop is 6
Current iteration of loop is 7
Current iteration of loop is 8
Current iteration of loop is 9

```

The most important thing is, that our loop starts with '0' value, so first line of results is 'Current iteration of loop is 0'.



## FOR Loop


Loop FOR works on similar rules as other loops. FOR loop is executed exact number of times and this number is known at the beginning - lower and upper limits are directly set in code. In every step in this example, loop is increment by 1.

Simple example:

```sql
DECLARE
v_counter NUMBER(2); --declaration of counter variable

BEGIN
  v_counter := 0; --point of start, first value of our iteration, execute of variable
 
  FOR v_counter IN 1..10 LOOP --The point, where lower and upper point of loop statement is declared - in this example, loop will be executed 10 times, start with value of 1
    
    dbms_output.put_line('Current iteration of loop is ' || v_counter); --show current iteration number in dbms script output
   
  END LOOP; --end of loop declaration
END;

```

And the result is:

```sql
Current iteration of loop is 1
Current iteration of loop is 2
Current iteration of loop is 3
Current iteration of loop is 4
Current iteration of loop is 5
Current iteration of loop is 6
Current iteration of loop is 7
Current iteration of loop is 8
Current iteration of loop is 9
Current iteration of loop is 10

```

Loop FOR has additional property, which is working in reverse. Using additional word 'REVERSE' in declaration of lower and upper limit of loop allow to do that. Every execution of loop decrement value of v_counter by 1.

Example:

```sql
DECLARE
v_counter NUMBER(2); --declaration of counter variable

BEGIN
  v_counter := 0; --point of start
 
  FOR v_counter IN REVERSE 1..10 LOOP
    
    dbms_output.put_line('Current iteration of loop is ' || v_counter); --show current iteration number in dbms script output
   
  END LOOP; --end of loop declaration
END;

```

And the result:

```sql
Current iteration of loop is 10
Current iteration of loop is 9
Current iteration of loop is 8
Current iteration of loop is 7
Current iteration of loop is 6
Current iteration of loop is 5
Current iteration of loop is 4
Current iteration of loop is 3
Current iteration of loop is 2
Current iteration of loop is 1

```



#### Syntax


1. LOOP
1. [statements];
1. EXIT WHEN [condition for exit loop];
1. END LOOP;

