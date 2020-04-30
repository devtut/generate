---
metaTitle: "Cursors"
description: "Basic Forward Only Cursor, Rudimentary cursor syntax"
---

# Cursors



## Basic Forward Only Cursor


Normally you would want to avoid using cursors as they can have negative impacts on performance. However in some special cases you may need to loop through your data record by record and perform some action.

```sql
DECLARE @orderId AS INT

-- here we are creating our cursor, as a local cursor and only allowing 
-- forward operations
DECLARE rowCursor CURSOR LOCAL FAST_FORWARD FOR
    -- this is the query that we want to loop through record by record
    SELECT [OrderId]
    FROM [dbo].[Orders]

-- first we need to open the cursor
OPEN rowCursor

-- now we will initialize the cursor by pulling the first row of data, in this example the [OrderId] column,
-- and storing the value into a variable called @orderId
FETCH NEXT FROM rowCursor INTO @orderId

-- start our loop and keep going until we have no more records to loop through
WHILE @@FETCH_STATUS = 0 
BEGIN

    PRINT @orderId
    
    -- this is important, as it tells SQL Server to get the next record and store the [OrderId] column value into the @orderId variable
    FETCH NEXT FROM rowCursor INTO @orderId

END

-- this will release any memory used by the cursor
CLOSE rowCursor
DEALLOCATE rowCursor

```



## Rudimentary cursor syntax


A simple cursor syntax, operating on a few example test rows:

```sql
/* Prepare test data */
DECLARE @test_table TABLE
(
    Id INT,
    Val VARCHAR(100)
);
INSERT INTO @test_table(Id, Val)
VALUES 
    (1, 'Foo'), 
    (2, 'Bar'), 
    (3, 'Baz');
/* Test data prepared */

/* Iterator variable @myId, for example sake */
DECLARE @myId INT;

/* Cursor to iterate rows and assign values to variables */
DECLARE myCursor CURSOR FOR
    SELECT Id
    FROM @test_table;

/* Start iterating rows */
OPEN myCursor;
FETCH NEXT FROM myCursor INTO @myId;

/* @@FETCH_STATUS global variable will be 1 / true until there are no more rows to fetch */
WHILE @@FETCH_STATUS = 0
BEGIN

    /* Write operations to perform in a loop here. Simple SELECT used for example */
    SELECT Id, Val
    FROM @test_table 
    WHERE Id = @myId;

    /* Set variable(s) to the next value returned from iterator; this is needed otherwise the cursor will loop infinitely. */
    FETCH NEXT FROM myCursor INTO @myId;
END
/* After all is done, clean up */
CLOSE myCursor;
DEALLOCATE myCursor;

```

Results from SSMS. Note that these are all separate queries, they are in no way unified. Notice how the query engine processes each iteration one by one instead of as a set.

|Id|Val
|------
|1|Foo
|(1 row(s) affected)|

|Id|Val
|------
|2|Bar
|(1 row(s) affected)|

|Id|Val
|------
|3|Baz
|(1 row(s) affected)|



#### Syntax


<li>DECLARE cursor_name CURSOR [ LOCAL | GLOBAL ]
<ul>
<li>[ FORWARD_ONLY | SCROLL ]<br />
[ STATIC | KEYSET | DYNAMIC | FAST_FORWARD ]<br />
[ READ_ONLY | SCROLL_LOCKS | OPTIMISTIC ]<br />
[ TYPE_WARNING ]</li>
- FOR select_statement
- [ FOR UPDATE [ OF column_name [ ,...n ] ] ]



#### Remarks


Normally you would want to avoid using cursors as they can have negative impacts on performance. However in some special cases you may need to loop through your data record by record and perform some action.

