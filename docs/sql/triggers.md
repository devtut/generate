---
metaTitle: "Triggers"
description: "CREATE TRIGGER, Use Trigger to manage a Recycle Bin for deleted items"
---

# Triggers



## CREATE TRIGGER


This example creates a trigger that inserts a record to a second table (MyAudit) after a record is inserted into the table the trigger is defined on (MyTable). Here the "inserted" table is a special table used by Microsoft SQL Server to store affected rows during INSERT and UPDATE statements; there is also a special "deleted" table that performs the same function for DELETE statements.

```sql
CREATE TRIGGER MyTrigger
    ON MyTable
    AFTER INSERT

AS

BEGIN
    -- insert audit record to MyAudit table
    INSERT INTO MyAudit(MyTableId, User)
    (SELECT MyTableId, CURRENT_USER FROM inserted)
END

```



## Use Trigger to manage a "Recycle Bin" for deleted items


```sql
CREATE TRIGGER BooksDeleteTrigger
    ON MyBooksDB.Books
    AFTER DELETE
AS
  INSERT INTO BooksRecycleBin
    SELECT *
    FROM deleted;
GO

```

