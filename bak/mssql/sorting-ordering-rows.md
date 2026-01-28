---
metaTitle: "Microsoft SQL Server - Sorting/ordering rows"
description: "Basics, Order by Case"
---

# Sorting/ordering rows



## Basics


First, let's setup the example table.

```sql
-- Create a table as an example
CREATE TABLE SortOrder
(
    ID INT IDENTITY PRIMARY KEY,
    [Text] VARCHAR(256)
)
GO

-- Insert rows into the table
INSERT INTO SortOrder ([Text]) 
SELECT ('Lorem ipsum dolor sit amet, consectetur adipiscing elit')
UNION ALL SELECT ('Pellentesque eu dapibus libero')
UNION ALL SELECT ('Vestibulum et consequat est, ut hendrerit ligula')
UNION ALL SELECT ('Suspendisse sodales est congue lorem euismod, vel facilisis libero pulvinar')
UNION ALL SELECT ('Suspendisse lacus est, aliquam at varius a, fermentum nec mi')
UNION ALL SELECT ('Praesent tincidunt tortor est, nec consequat dolor malesuada quis')
UNION ALL SELECT ('Quisque at tempus arcu')
GO

```

Remember that when retrieving data, if you don't specify a row ordering clause (ORDER BY) SQL server does not guarantee the sorting (order of the columns) **at any time**. Really, at any time. And there's no point arguing about that, it has been shown literaly thousands of times and all over the internet.

No ORDER BY == no sorting. End of story.

```sql
-- It may seem the rows are sorted by identifiers, 
-- but there is really no way of knowing if it will always work.
-- And if you leave it like this in production, Murphy gives you a 100% that it wont.
SELECT * FROM SortOrder
GO

```

There are two directions data can be ordered by:

- ascending (moving upwards), using ASC
- descending (moving downwards), using DESC

```sql
-- Ascending - upwards
SELECT * FROM SortOrder ORDER BY ID ASC
GO

-- Ascending is default
SELECT * FROM SortOrder ORDER BY ID
GO

-- Descending - downwards
SELECT * FROM SortOrder ORDER BY ID DESC
GO

```

When ordering by the textual column ((n)char or (n)varchar), pay attention that the order respects the collation. For more information on collation look up for the topic.

Ordering and sorting of data can consume resources. This is where properly created indexes come handy. For more information on indexes look up for the topic.

There is a possibility to pseudo-randomize the order of rows in your resultset. Just force the ordering to appear nondeterministic.

```sql
SELECT * FROM SortOrder ORDER BY CHECKSUM(NEWID())
GO

```

Ordering can be remembered in a stored procedure, and that's the way you should do it if it is the last step of manipulating the rowset before showing it to the end user.

```sql
CREATE PROCEDURE GetSortOrder
AS
    SELECT * 
    FROM SortOrder 
    ORDER BY ID DESC
GO

EXEC GetSortOrder
GO

```

There is a limited (and hacky) support for ordering in the SQL Server views as well, but be encouraged NOT to use it.

```sql
/* This may or may not work, and it depends on the way 
   your SQL Server and updates are installed */
CREATE VIEW VwSortOrder1
AS
    SELECT TOP 100 PERCENT * 
    FROM SortOrder 
    ORDER BY ID DESC
GO

SELECT * FROM VwSortOrder1
GO

-- This will work, but hey... should you really use it?
CREATE VIEW VwSortOrder2
AS
    SELECT TOP 99999999 * 
    FROM SortOrder 
    ORDER BY ID DESC
GO

SELECT * FROM VwSortOrder2
GO

```

For ordering you can either use column names, aliases or column numbers in your ORDER BY.

```sql
SELECT * 
FROM SortOrder 
ORDER BY [Text]

-- New resultset column aliased as 'Msg', feel free to use it for ordering
SELECT ID, [Text] + ' (' + CAST(ID AS nvarchar(10)) + ')' AS Msg
FROM SortOrder 
ORDER BY Msg

-- Can be handy if you know your tables, but really NOT GOOD for production
SELECT * 
FROM SortOrder 
ORDER BY 2

```

I advise against using the numbers in your code, except if you want to forget about it the moment after you execute it.



## Order by Case


If you want to sort your data numerically or alphabetically, you can simply use `order by [column]`.  If you want to sort using a custom hierarchy, use a case statement.

```sql
Group
-----
Total
Young
MiddleAge
Old
Male
Female

```

Using a basic `order by`:

```sql
Select * from MyTable
Order by Group

```

returns an alphabetical sort, which isn't always desirable:

```sql
Group
-----
Female
Male
MiddleAge
Old    
Total
Young

```

Adding a 'case' statement, assigning ascending numerical values in the order you want your data sorted:

```sql
Select * from MyTable
Order by case Group
    when 'Total' then 10
    when 'Male' then 20
    when 'Female' then 30
    when 'Young' then 40
    when 'MiddleAge' then 50
    when 'Old' then 60
    end

```

returns data in the order specified:

```sql
Group
-----
Total
Male
Female
Young
MiddleAge
Old

```

