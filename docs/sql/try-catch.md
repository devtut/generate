---
metaTitle: "SQL - TRY/CATCH"
description: "Transaction In a TRY/CATCH"
---

# TRY/CATCH



## Transaction In a TRY/CATCH


This will rollback both inserts due to an invalid datetime:

```sql
BEGIN TRANSACTION
BEGIN TRY
    INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
    VALUES (5.2, GETDATE(), 1)
    INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
    VALUES (5.2, 'not a date', 1)
    COMMIT TRANSACTION
END TRY
BEGIN CATCH
    THROW
    ROLLBACK TRANSACTION
END CATCH

```

This will commit both inserts:

```sql
BEGIN TRANSACTION
BEGIN TRY
    INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
    VALUES (5.2, GETDATE(), 1)
    INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
    VALUES (5.2, GETDATE(), 1)
    COMMIT TRANSACTION
END TRY
BEGIN CATCH
    THROW
    ROLLBACK TRANSACTION
END CATCH

```



#### Remarks


TRY/CATCH is a language construct specific to MS SQL Server's T-SQL.

It allows error handling within T-SQL, similar to that seen in .NET code.

