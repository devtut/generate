---
metaTitle: "Transactions"
description: "Simple Transaction, Rollback Transaction"
---

# Transactions



## Simple Transaction


```sql
BEGIN TRANSACTION
    INSERT INTO DeletedEmployees(EmployeeID, DateDeleted, User)
        (SELECT 123, GetDate(), CURRENT_USER);
    DELETE FROM Employees WHERE EmployeeID = 123;
COMMIT TRANSACTION

```



## Rollback Transaction


When something fails in your transaction code and you want to undo it, you can rollback your transaction:

```sql
BEGIN TRY
    BEGIN TRANSACTION
        INSERT INTO Users(ID, Name, Age)
        VALUES(1, 'Bob', 24)
        
        DELETE FROM Users WHERE Name = 'Todd'
   COMMIT TRANSACTION
END TRY
BEGIN CATCH
   ROLLBACK TRANSACTION
END CATCH

```



#### Remarks


A transaction is a logical unit of work containing one or more steps, each of which must complete successfully in order for the transaction to commit to the database. If there are errors, then all of the data modifications are erased and the database is rolled back to its initial state at the start of the transaction.

