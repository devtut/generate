---
metaTitle: "Microsoft SQL Server - Transaction handling"
description: "basic transaction skeleton with error handling"
---

# Transaction handling



## basic transaction skeleton with error handling


```sql
BEGIN TRY -- start error handling
    BEGIN TRANSACTION; -- from here on transactions (modifictions) are not final
        -- start your statement(s) 
        select 42/0 as ANSWER  -- simple SQL Query with an error
        -- end your statement(s)
    COMMIT TRANSACTION; -- finalize all transactions (modifications)
END TRY   -- end error handling -- jump to end
BEGIN CATCH -- execute this IF an error occured
        ROLLBACK TRANSACTION; -- undo any transactions (modifications)
-- put together some information as a query
    SELECT 
        ERROR_NUMBER() AS ErrorNumber
        ,ERROR_SEVERITY() AS ErrorSeverity
        ,ERROR_STATE() AS ErrorState
        ,ERROR_PROCEDURE() AS ErrorProcedure
        ,ERROR_LINE() AS ErrorLine
        ,ERROR_MESSAGE() AS ErrorMessage;

END CATCH;  -- final line of error handling
GO -- execute previous code

```



#### Parameters


|Parameter|Details
|---|---|---|---
|transaction_name|for naming your transaction - useful with the parameter [**with mark**] which will allow a meaningfull logging -- case-sensitive (!)
|with mark ['description']|can be added to [**transaction_name**] and will store a mark in the log

