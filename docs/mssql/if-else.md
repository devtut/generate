---
metaTitle: "IF...ELSE"
description: "Single IF statement, Multiple IF Statements, Single IF..ELSE statement , Multiple IF... ELSE with final ELSE Statements, Multiple IF...ELSE Statements"
---

# IF...ELSE



## Single IF statement


Like most of the other programming languages,  T-SQL also supports IF..ELSE statements.

For example in the example below `1 = 1` is the expression, which evaluates to True and the control enters the `BEGIN..END` block and the Print statement prints the string `'One is equal to One'`

```sql
IF ( 1 = 1)  --<-- Some Expression 
 BEGIN
   PRINT 'One is equal to One'
 END

```



## Multiple IF Statements


We can use multiple IF statement to check multiple expressions totally independent from each other.

In the example below, each `IF` statement's expression is evaluated and if it is true the code inside the `BEGIN...END` block is executed. In this particular example, the First and Third expressions are true and only those print statements will be executed.

```sql
IF (1 = 1)  --<-- Some Expression      --<-- This is true 
BEGIN
    PRINT 'First IF is True'           --<-- this will be executed
END

IF (1 = 2)  --<-- Some Expression 
BEGIN
    PRINT 'Second IF is True'
END

IF (3 = 3)  --<-- Some Expression        --<-- This true 
BEGIN
    PRINT 'Thrid IF is True'             --<-- this will be executed
END

```



## Single IF..ELSE statement 


In a single `IF..ELSE` statement, if the expression evaluates to True in the `IF` statement the control enters the first `BEGIN..END` block and only the code inside that block gets executed , Else block is simply ignored.

On the other hand if the expression evaluates to `False` the `ELSE BEGIN..END` block gets executed and the control never enters the first `BEGIN..END` Block.

In the Example below the expression will evaluate to false and the Else block will be executed printing the string `'First expression was not true'`

```sql
IF ( 1 <> 1)  --<-- Some Expression
 BEGIN
     PRINT 'One is equal to One'
 END
ELSE 
 BEGIN
     PRINT 'First expression was not true'
 END

```



## Multiple IF... ELSE with final ELSE Statements


If we have Multiple `IF...ELSE IF` statements but we also want also want to execute some piece of code if none of expressions are evaluated to True , then we can simple add a final `ELSE` block which only gets executed if none of the `IF` or `ELSE IF` expressions are evaluated to true.

In the example below none of the `IF` or `ELSE IF` expression are True hence only `ELSE` block is executed and prints `'No other expression is true'`

```sql
IF ( 1 = 1 + 1 )
    BEGIN
       PRINT 'First If Condition'
    END 
ELSE IF (1 = 2)
    BEGIN
        PRINT 'Second If Else Block'
    END
ELSE IF (1 = 3)
    BEGIN
        PRINT 'Third If Else Block'
    END
ELSE 
    BEGIN
        PRINT 'No other expression is true'  --<-- Only this statement will be printed
    END

```



## Multiple IF...ELSE Statements


More often than not we need to check multiple expressions and take specific actions based on those expressions. This situation is handled using multiple `IF...ELSE IF` statements.

In this example all the expressions are evaluated from top to bottom. As soon as an expression evaluates to true, the code inside that block is executed. If no expression is evaluated to true, nothing gets executed.

```sql
IF (1 = 1 + 1)
BEGIN
    PRINT 'First If Condition'
END 
ELSE IF (1 = 2)
BEGIN
    PRINT 'Second If Else Block'
END
ELSE IF (1 = 3)
BEGIN
    PRINT 'Third If Else Block'
END
ELSE IF (1 = 1)      --<-- This is True
BEGIN
    PRINT 'Last Else Block'  --<-- Only this statement will be printed
END

```

