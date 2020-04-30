---
metaTitle: "Natively compiled modules (Hekaton)"
description: "Natively compiled stored procedure, Natively compiled scalar function, Native inline table value function"
---

# Natively compiled modules (Hekaton)



## Natively compiled stored procedure


In a procedure with native compilation, T-SQL code is compiled to dll and executed as native C code. To create a Native Compiled stored Procedure you need to:

- Use standard CREATE PROCEDURE syntax
- Set NATIVE_COMPILATION option in stored procedure definition
- Use SCHEMABINDING option in stored procedure definition
- Define EXECUTE AS OWNER option in stored procedure definition

Instead of standard BEGIN END block, you need to use BEGIN ATOMIC block:

```sql
BEGIN ATOMIC
   WITH (TRANSACTION ISOLATION LEVEL=SNAPSHOT, LANGUAGE='us_english')
   -- T-Sql code goes here
END

```

Example:

```sql
CREATE PROCEDURE usp_LoadMemOptTable (@maxRows INT, @FullName NVARCHAR(200))
WITH
    NATIVE_COMPILATION, 
    SCHEMABINDING, 
    EXECUTE AS OWNER
AS
BEGIN ATOMIC
WITH (TRANSACTION ISOLATION LEVEL=SNAPSHOT, LANGUAGE='us_english')
    DECLARE @i INT = 1
    WHILE @i <= @maxRows
    BEGIN
        INSERT INTO dbo.MemOptTable3 VALUES(@i, @FullName, GETDATE())
        SET @i = @i+1
    END
END
GO

```



## Natively compiled scalar function


Code in natively compiled function will be transformed into C code and compiled as dll. To create a Native Compiled scalar function you need to:

- Use standard CREATE FUNCTION syntax
- Set NATIVE_COMPILATION option in function definition
- Use SCHEMABINDING option in function definition

Instead of standard BEGIN END block, you need to use BEGIN ATOMIC block:

```sql
BEGIN ATOMIC
   WITH (TRANSACTION ISOLATION LEVEL=SNAPSHOT, LANGUAGE='us_english')
   -- T-Sql code goes here
END

```

Example:

```sql
CREATE FUNCTION [dbo].[udfMultiply]( @v1 int, @v2 int )   
RETURNS bigint 
WITH NATIVE_COMPILATION, SCHEMABINDING  
AS   
BEGIN ATOMIC WITH (TRANSACTION ISOLATION LEVEL = SNAPSHOT, LANGUAGE = N'English')  
  
    DECLARE @ReturnValue bigint;  
    SET @ReturnValue = @v1 * @v2;         
  
    RETURN (@ReturnValue);    
END 

-- usage sample:
SELECT dbo.udfMultiply(10, 12)

```



## Native inline table value function


Native compiled table value function returns table as result. Code in natively compiled function will be transformed into C code and compiled as dll.  Only inline table valued functions are supported in version 2016. To create a native table value function you need to:

- Use standard CREATE FUNCTION syntax
- Set NATIVE_COMPILATION option in function definition
- Use SCHEMABINDING option in function definition

Instead of standard BEGIN END block, you need to use BEGIN ATOMIC block:

```sql
BEGIN ATOMIC
   WITH (TRANSACTION ISOLATION LEVEL=SNAPSHOT, LANGUAGE='us_english')
   -- T-Sql code goes here
END

```

Example:

```sql
CREATE FUNCTION [dbo].[udft_NativeGetBusinessDoc]
(
   @RunDate VARCHAR(25)
)
RETURNS TABLE
WITH SCHEMABINDING,
     NATIVE_COMPILATION
AS
     RETURN
(
    SELECT BusinessDocNo,
           ProductCode,
           UnitID,               
           ReasonID,
           PriceID,
           RunDate,
           ReturnPercent,
           Qty,
           RewardAmount,
           ModifyDate,
           UserID
    FROM dbo.[BusinessDocDetail_11]
    WHERE RunDate >= @RunDate
);

```

