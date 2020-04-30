---
metaTitle: "CASE Statement"
description: "Simple CASE statement, Searched CASE statement"
---

# CASE Statement



## Simple CASE statement


In a simple case statement, one value or variable is checked against multiple possible answers. The code below is an example of a simple case statement:

```sql
SELECT CASE DATEPART(WEEKDAY, GETDATE())
    WHEN 1 THEN 'Sunday'
    WHEN 2 THEN 'Monday' 
    WHEN 3 THEN 'Tuesday' 
    WHEN 4 THEN 'Wednesday' 
    WHEN 5 THEN 'Thursday' 
    WHEN 6 THEN 'Friday' 
    WHEN 7 THEN 'Saturday' 
END

```



## Searched CASE statement


In a Searched Case statement, each option can test one or more values independently. The code below is an example of a searched case statement:

```sql
DECLARE @FirstName varchar(30) = 'John'
DECLARE @LastName varchar(30) = 'Smith'

SELECT CASE
    WHEN LEFT(@FirstName, 1) IN ('a','e','i','o','u')
        THEN 'First name starts with a vowel'
    WHEN LEFT(@LastName, 1) IN ('a','e','i','o','u')
        THEN 'Last name starts with a vowel'
    ELSE
        'Neither name starts with a vowel'
END

```



#### Remarks


Above example is just to show the syntax for using case statements in SQL Server with day of week example. Although same can output can be achieved by using "SELECT DATENAME(WEEKDAY, GETDATE())" as well.

