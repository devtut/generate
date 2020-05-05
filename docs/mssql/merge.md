---
metaTitle: "Microsoft SQL Server - MERGE"
description: "MERGE to Insert / Update / Delete, Merge Using CTE Source, MERGE using Derived Source Table, Merge Example - Synchronize Source And Target Table, Merge using EXCEPT"
---

# MERGE


Starting with SQL Server 2008, it is possible to perform insert, update, or delete operations in a single statement using the MERGE statement.

The MERGE statement allows you to join a data source with a target table or view, and then perform multiple actions against the target based on the results of that join.



## MERGE to Insert / Update / Delete


```sql
MERGE INTO targetTable

USING sourceTable 
ON (targetTable.PKID = sourceTable.PKID)

WHEN MATCHED AND (targetTable.PKID > 100) THEN
    DELETE

WHEN MATCHED AND (targetTable.PKID <= 100) THEN 
    UPDATE SET 
        targetTable.ColumnA = sourceTable.ColumnA, 
        targetTable.ColumnB = sourceTable.ColumnB

WHEN NOT MATCHED THEN
    INSERT (ColumnA, ColumnB) VALUES (sourceTable.ColumnA, sourceTable.ColumnB);

WHEN NOT MATCHED BY SOURCE THEN
    DELETE
; --< Required

```

Description:

- `MERGE INTO targetTable` - table to be modified
- `USING sourceTable` - source of data (can be table or view or table valued function)
- `ON ...` - join condition between `targetTable` and `sourceTable`.
- `WHEN MATCHED` - actions to take when a match is found
<li>
<ul>
- `AND (targetTable.PKID > 100)` - additional condition(s) that must be satisfied in order for the action to be taken

Comments:

If a specific action is not needed then omit the condition e.g. removing `WHEN NOT MATCHED THEN INSERT` will prevent records from being inserted

Merge statement requires a terminating semicolon.

Restrictions:

- `WHEN MATCHED` does not allow `INSERT` action
- `UPDATE` action can update a row only once. This implies that the join condition must produce unique matches.



## Merge Using CTE Source


```sql
WITH SourceTableCTE AS
(
    SELECT * FROM SourceTable
)
MERGE  
 TargetTable AS target
USING SourceTableCTE AS source  
ON (target.PKID = source.PKID)
WHEN MATCHED THEN     
    UPDATE SET target.ColumnA = source.ColumnA
WHEN NOT MATCHED THEN
    INSERT (ColumnA) VALUES (Source.ColumnA);

```



## MERGE using Derived Source Table


```sql
MERGE INTO TargetTable  AS Target  
USING (VALUES (1,'Value1'), (2, 'Value2'), (3,'Value3'))  
       AS Source (PKID, ColumnA)  
ON Target.PKID = Source.PKID 
WHEN MATCHED THEN 
    UPDATE SET target.ColumnA= source.ColumnA
WHEN NOT MATCHED THEN
    INSERT (PKID, ColumnA) VALUES (Source.PKID, Source.ColumnA);

```



## Merge Example - Synchronize Source And Target Table


To Illustrate the MERGE Statement, consider the following two tables -

<li>
**dbo.Product** : This table contains information about the product that company is currently selling
</li>
<li>
**dbo.ProductNew**: This table contains information about the product that the company will sell in the future.
</li>

The following T-SQL will create and populate these two tables

```sql
IF OBJECT_id(N'dbo.Product',N'U') IS NOT NULL 
DROP TABLE dbo.Product
GO

CREATE TABLE dbo.Product (
ProductID INT PRIMARY KEY,
ProductName NVARCHAR(64),
PRICE MONEY
)

IF OBJECT_id(N'dbo.ProductNew',N'U') IS NOT NULL 
DROP TABLE dbo.ProductNew
GO

CREATE TABLE dbo.ProductNew (
ProductID INT PRIMARY KEY,
ProductName NVARCHAR(64),
PRICE MONEY
)

INSERT INTO dbo.Product VALUES(1,'IPod',300)
,(2,'IPhone',400)
,(3,'ChromeCast',100)
,(4,'raspberry pi',50)

INSERT INTO dbo.ProductNew VALUES(1,'Asus Notebook',300)
,(2,'Hp Notebook',400)
,(3,'Dell Notebook',100)
,(4,'raspberry pi',50)

```

Now, Suppose we want to synchoronize the dbo.Product Target Table with the dbo.ProductNew table. Here is the criterion for this task:

<li>
Product that exist in both the dbo.ProductNew source table and the dbo.Product target table are updated in the dbo.Product target table with new new Products.
</li>
<li>
Any product in the dbo.ProductNew source table that do not exist in the dob.Product target table are inserted into the dbo.Product  target table.
</li>
<li>
<p>Any Product in the dbo.Product target table that do not exist in the dbo.ProductNew source table must be deleted from the dbo.Product target table.
Here is the MERGE statement to perform this task.</p>
</li>

```sql
MERGE dbo.Product AS SourceTbl 
USING dbo.ProductNew AS TargetTbl ON (SourceTbl.ProductID = TargetTbl.ProductID)
WHEN MATCHED 
            AND SourceTbl.ProductName <> TargetTbl.ProductName
            OR SourceTbl.Price <> TargetTbl.Price
    THEN UPDATE SET SourceTbl.ProductName = TargetTbl.ProductName,
                SourceTbl.Price = TargetTbl.Price
WHEN NOT MATCHED 
    THEN INSERT (ProductID, ProductName, Price)
         VALUES (TargetTbl.ProductID, TargetTbl.ProductName, TargetTbl.Price)
WHEN NOT MATCHED BY SOURCE 
    THEN DELETE
OUTPUT $action, INSERTED.*, DELETED.*;

```

Note:Semicolon must be present in the end of MERGE statement.
[<img src="https://i.stack.imgur.com/GULoi.jpg" alt="enter image description here" />](https://i.stack.imgur.com/GULoi.jpg)



## Merge using EXCEPT


Use EXCEPT to prevent updates to unchanged records

```sql
MERGE TargetTable targ
USING SourceTable AS src
    ON src.id = targ.id
WHEN MATCHED
    AND EXISTS (
        SELECT src.field
        EXCEPT
        SELECT targ.field
        )
    THEN
        UPDATE
        SET field = src.field
WHEN NOT MATCHED BY TARGET
    THEN
        INSERT (
            id
            ,field
            )
        VALUES (
            src.id
            ,src.field
            )
WHEN NOT MATCHED BY SOURCE
    THEN
        DELETE;

```



#### Syntax


<li>As per MSDN - [https://msdn.microsoft.com/en-us/library/bb510625.aspx](https://msdn.microsoft.com/en-us/library/bb510625.aspx)
[ WITH <common_table_expression> [,...n] ] MERGE [ TOP ( expression )
[ PERCENT ] ] [ INTO ] <target_table> [ WITH ( <merge_hint> ) ] [ [
AS ] table_alias ] USING <table_source> ON <merge_search_condition> [
WHEN MATCHED [ AND <clause_search_condition> ] THEN <merge_matched> ]
[ ...n ] [ WHEN NOT MATCHED [ BY TARGET ] [ AND
<clause_search_condition> ] THEN <merge_not_matched> ] [ WHEN NOT
MATCHED BY SOURCE [ AND <clause_search_condition> ] THEN
<merge_matched> ] [ ...n ] [ <output_clause> ] [ OPTION (
<query_hint> [ ,...n ] ) ] ; <target_table> ::= { [ database_name .
schema_name . | schema_name . ] target_table } <merge_hint>::= { { [
<table_hint_limited> [ ,...n ] ] [ [ , ] INDEX ( index_val [ ,...n ]
) ] } } <table_source> ::= { table_or_view_name [ [ AS ] table_alias
] [ <tablesample_clause> ] [ WITH ( table_hint [ [ , ]...n ] ) ] |
rowset_function [ [ AS ] table_alias ] [ ( bulk_column_alias [ ,...n
] ) ] | user_defined_function [ [ AS ] table_alias ] | OPENXML
<openxml_clause> | derived_table [ AS ] table_alias [ ( column_alias
[ ,...n ] ) ] | <joined_table> | <pivoted_table> | <unpivoted_table>
} <merge_search_condition> ::= <search_condition> <merge_matched>::=
{ UPDATE SET <set_clause> | DELETE } <set_clause>::= SET {
column_name = { expression | DEFAULT | NULL } | { udt_column_name.{ {
property_name = expression | field_name = expression } | method_name
( argument [ ,...n ] ) } } | column_name { .WRITE ( expression ,
@Offset , @Length ) } | @variable = expression | @variable = column =
expression | column_name { += | -= | *= | /= | %= | &= | ^= | |= }
expression | @variable { += | -= | *= | /= | %= | &= | ^= | |= }
expression | @variable = column { += | -= | *= | /= | %= | &= | ^= |
|= } expression } [ ,...n ] <merge_not_matched>::= { INSERT [ (
column_list ) ] { VALUES ( values_list ) | DEFAULT VALUES } }
<clause_search_condition> ::= <search_condition> ::= { [ NOT ] | (
<search_condition> ) } [ { AND | OR } [ NOT ] { | (
<search_condition> ) } ] [ ,...n ] ::= { expression { = | < > | ! = |
<blockquote>
<p>| > = | ! > | < | < = | ! < } expression | string_expression [ NOT ] LIKE string_expression [ ESCAPE 'escape_character' ] | expression [
NOT ] BETWEEN expression AND expression | expression IS [ NOT ] NULL
| CONTAINS ( { column | * } , '< contains_search_condition >' ) |
FREETEXT ( { column | * } , 'freetext_string' ) | expression [ NOT ]
IN ( subquery | expression [ ,...n ] ) | expression { = | < > | ! = |
| > = | ! > | < | < = | ! < } { ALL | SOME | ANY} ( subquery ) | EXISTS ( subquery ) } <output_clause>::= { [ OUTPUT <dml_select_list>
INTO { @table_variable | output_table } [ (column_list) ] ] [ OUTPUT
<dml_select_list> ] } <dml_select_list>::= { <column_name> |
scalar_expression } [ [AS] column_alias_identifier ] [ ,...n ]
<column_name> ::= { DELETED | INSERTED | from_table_name } . { * |
column_name } | $action</p>
</blockquote>
</li>



#### Remarks


Performs insert, update, or delete operations on a target table based on the results of a join with a source table. For example, you can synchronize two tables by inserting, updating, or deleting rows in one table based on differences found in the other table.

