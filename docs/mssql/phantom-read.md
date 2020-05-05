---
metaTitle: "Microsoft SQL Server - PHANTOM read"
description: "Isolation level READ UNCOMMITTED"
---

# PHANTOM read




## Isolation level READ UNCOMMITTED


Create a sample table on a sample database

```sql
CREATE TABLE [dbo].[Table_1](
    [Id] [int] IDENTITY(1,1) NOT NULL,
    [title] [varchar](50) NULL,
 CONSTRAINT [PK_Table_1] PRIMARY KEY CLUSTERED 
(
    [Id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

```

Now open a First query editor (on the database) insert the code below, and execute (**do not touch the --rollback**) in this case you insert a row on DB but do **not** commit changes.

```sql
begin tran

INSERT INTO Table_1 values('Title 1')

SELECT * FROM [Test].[dbo].[Table_1]

--rollback

```

Now open a Second Query Editor (on the database), insert the code below and execute

```sql
begin tran

set transaction isolation level READ UNCOMMITTED

SELECT * FROM [Test].[dbo].[Table_1]

```

You may notice that on second editor you can see the newly created row (but not committed) from first transaction. On first editor execute the rollback (select the rollback word and execute).

```sql
-- Rollback the first transaction
rollback

```

Execute the query on second editor and you see that the record disappear (phantom read), this occurs because you tell, to the 2nd transaction to get all rows, also the uncommitteds.

This occurs when you change the isolation level with

```sql
set transaction isolation level READ UNCOMMITTED

```



#### Remarks


You can read the various `ISOLATION LEVEL` on [MSDN](https://msdn.microsoft.com/en-US/library/ms173763.aspx)

