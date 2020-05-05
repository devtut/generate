---
metaTitle: "Microsoft SQL Server - Trigger"
description: "DML Triggers, Types and classifications of Trigger"
---

# Trigger


A trigger is a special type of stored procedure, which is executed automatically after an event occurs. There are two types of triggers: Data Definition Language Triggers and Data Manipulation Language Triggers.

It is usually bound to a table and fires automatically. You cannot explicitly call any trigger.



## DML Triggers


DML Triggers are fired as a response to dml statements (`insert`, `update` or `delete`).<br />
A dml trigger can be created to address one or more dml events for a single table or view.
This means that a single dml trigger can handle inserting, updating and deleting records from a specific table or view, but in can only handle data being changed on that single table or view.

DML Triggers provides access to `inserted` and `deleted` tables that holds information about the data that was / will be affected by the insert, update or delete statement that fired the trigger.

**Note** that DML triggers are statement based, not row based. This means that if the statement effected more then one row, the inserted or deleted tables will contain more then one row.

Examples:

```sql
CREATE TRIGGER tblSomething_InsertOrUpdate ON tblSomething  
FOR INSERT
AS

    INSERT INTO tblAudit (TableName, RecordId, Action)
    SELECT 'tblSomething', Id, 'Inserted'
    FROM Inserted

GO

CREATE TRIGGER tblSomething_InsertOrUpdate ON tblSomething  
FOR UPDATE
AS

    INSERT INTO tblAudit (TableName, RecordId, Action)
    SELECT 'tblSomething', Id, 'Updated'
    FROM Inserted 

GO

CREATE TRIGGER tblSomething_InsertOrUpdate ON tblSomething  
FOR DELETE
AS

    INSERT INTO tblAudit (TableName, RecordId, Action)
    SELECT 'tblSomething', Id, 'Deleted'
    FROM Deleted

GO

```

All the examples above will add records to tblAudit whenever a record is added, deleted or updated in tblSomething.



## Types and classifications of Trigger


In SQL Server, there are two categories of triggers: DDL Triggers and DML Triggers.

DDL Triggers are fired in response to Data Definition Language (DDL) events. These events primarily correspond to Transact-SQL statements that start with the keywords `CREATE`, `ALTER` and `DROP`.

DML Triggers are fired in response to Data Manipulation Language (DML) events. These events corresponds to Transact-SQL statements that start with the keywords `INSERT`, `UPDATE` and `DELETE`.

DML triggers are classified into two main types:

<li>
After Triggers (for triggers)
<ul>
1. AFTER INSERT Trigger.
1. AFTER UPDATE Trigger.
1. AFTER DELETE Trigger.
</ul>
</li>
<li>
Instead of triggers
<ul>
1. INSTEAD OF INSERT Trigger.
1. INSTEAD OF UPDATE Trigger.
1. INSTEAD OF DELETE Trigger.
</ul>
</li>

- INSTEAD OF INSERT Trigger.
- INSTEAD OF UPDATE Trigger.
- INSTEAD OF DELETE Trigger.

