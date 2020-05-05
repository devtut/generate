---
metaTitle: "Microsoft SQL Server - PIVOT / UNPIVOT"
description: "Simple PIVOT & UNPIVOT (T-SQL), Dynamic PIVOT, Simple Pivot - Static Columns"
---

# PIVOT / UNPIVOT



## Simple PIVOT & UNPIVOT (T-SQL)


Below is a simple example which shows average item's price of each item per weekday.

First, suppose we have a table which keeps daily records of all items' prices.

```sql
CREATE TABLE tbl_stock(item NVARCHAR(10), weekday NVARCHAR(10), price INT);

INSERT INTO tbl_stock VALUES 
('Item1', 'Mon', 110), ('Item2', 'Mon', 230), ('Item3', 'Mon', 150), 
('Item1', 'Tue', 115), ('Item2', 'Tue', 231), ('Item3', 'Tue', 162), 
('Item1', 'Wed', 110), ('Item2', 'Wed', 240), ('Item3', 'Wed', 162), 
('Item1', 'Thu', 109), ('Item2', 'Thu', 228), ('Item3', 'Thu', 145), 
('Item1', 'Fri', 120), ('Item2', 'Fri', 210), ('Item3', 'Fri', 125),
('Item1', 'Mon', 122), ('Item2', 'Mon', 225), ('Item3', 'Mon', 140),
('Item1', 'Tue', 110), ('Item2', 'Tue', 235), ('Item3', 'Tue', 154),
('Item1', 'Wed', 125), ('Item2', 'Wed', 220), ('Item3', 'Wed', 142);

```

The table should look like below:

```sql
+========+=========+=======+
|   item | weekday | price |
+========+=========+=======+
|  Item1 |    Mon  |   110 |
+--------+---------+-------+
|  Item2 |    Mon  |   230 |
+--------+---------+-------+
|  Item3 |    Mon  |   150 |
+--------+---------+-------+
|  Item1 |    Tue  |   115 |
+--------+---------+-------+
|  Item2 |    Tue  |   231 |
+--------+---------+-------+
|  Item3 |    Tue  |   162 |
+--------+---------+-------+
|          . . .           |
+--------+---------+-------+
|  Item2 |    Wed  |   220 |
+--------+---------+-------+
|  Item3 |    Wed  |   142 |
+--------+---------+-------+

```

In order to perform aggregation which is to find the average price per item for each week day, we are going to use the relational operator `PIVOT` to rotate the column `weekday` of table-valued expression into aggregated row values as below:

```sql
SELECT * FROM tbl_stock
PIVOT ( 
    AVG(price) FOR weekday IN ([Mon], [Tue], [Wed], [Thu], [Fri])
) pvt;

```

Result:

```sql
+--------+------+------+------+------+------+
|  item  |  Mon |  Tue |  Wed |  Thu |  Fri |
+--------+------+------+------+------+------+
|  Item1 |  116 |  112 |  117 |  109 |  120 |
|  Item2 |  227 |  233 |  230 |  228 |  210 |
|  Item3 |  145 |  158 |  152 |  145 |  125 |
+--------+------+------+------+------+------+

```

Lastly, in order to perform the reverse operation of `PIVOT`, we can use the relational operator `UNPIVOT` to rotate columns into rows as below:

```sql
SELECT * FROM tbl_stock
PIVOT ( 
    AVG(price) FOR weekday IN ([Mon], [Tue], [Wed], [Thu], [Fri])
) pvt
UNPIVOT ( 
    price FOR weekday IN ([Mon], [Tue], [Wed], [Thu], [Fri])
) unpvt;

```

Result:

```sql
+=======+========+=========+
|  item |  price | weekday |
+=======+========+=========+
| Item1 |    116 |     Mon |
+-------+--------+---------+
| Item1 |    112 |     Tue |
+-------+--------+---------+
| Item1 |    117 |     Wed |
+-------+--------+---------+
| Item1 |    109 |     Thu |
+-------+--------+---------+
| Item1 |    120 |     Fri |
+-------+--------+---------+
| Item2 |    227 |     Mon |
+-------+--------+---------+
| Item2 |    233 |     Tue |
+-------+--------+---------+
| Item2 |    230 |     Wed |
+-------+--------+---------+
| Item2 |    228 |     Thu |
+-------+--------+---------+
| Item2 |    210 |     Fri |
+-------+--------+---------+
| Item3 |    145 |     Mon |
+-------+--------+---------+
| Item3 |    158 |     Tue |
+-------+--------+---------+
| Item3 |    152 |     Wed |
+-------+--------+---------+
| Item3 |    145 |     Thu |
+-------+--------+---------+
| Item3 |    125 |     Fri |
+-------+--------+---------+

```



## Dynamic PIVOT


One problem with the `PIVOT` query is that you have to specify all values inside the `IN` selection if you want to see them as columns.
A quick way to circumvent this problem is to create a dynamic IN selection making your `PIVOT` dynamic.

For demonstration we will use a table `Books` in a `Bookstore`’s database. We assume that the table is quite de-normalised and has following columns

```sql
Table: Books
-----------------------------
BookId (Primary Key Column)
Name
Language
NumberOfPages
EditionNumber
YearOfPrint
YearBoughtIntoStore
ISBN
AuthorName
Price
NumberOfUnitsSold

```

Creation script for the table will be like:

```sql
CREATE TABLE [dbo].[BookList](
      [BookId] [int] NOT NULL,
      [Name] [nvarchar](100) NULL,
      [Language] [nvarchar](100) NULL,
      [NumberOfPages] [int] NULL,
      [EditionNumber] [nvarchar](10) NULL,
      [YearOfPrint] [int] NULL,
      [YearBoughtIntoStore] [int] NULL,
[NumberOfBooks] [int] NULL,
[ISBN] [nvarchar](30) NULL,
      [AuthorName] [nvarchar](200) NULL,
      [Price] [money] NULL,
      [NumberOfUnitsSold] [int] NULL,
 CONSTRAINT [PK_BookList] PRIMARY KEY CLUSTERED
(
      [BookId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

```

Now if we need to query on the database and figure out number of books in English, Russian, German, Hindi, Latin languages bought into the bookstore every year and present our output in a small report format, we can use PIVOT query like this

```sql
SELECT * FROM
  (
   SELECT YearBoughtIntoStore AS [Year Bought],[Language], NumberOfBooks
   FROM BookList
  ) sourceData 
 PIVOT
  (
  SUM(NumberOfBooks)
  FOR [Language] IN (English, Russian, German, Hindi, Latin)
  ) pivotrReport

```

Special case is when we do not have a full list of the languages, so we'll use dynamic SQL like below

```sql
DECLARE @query VARCHAR(4000)
DECLARE @languages VARCHAR(2000)
SELECT @languages =
        STUFF((SELECT DISTINCT '],['+LTRIM([Language])FROM [dbo].[BookList]
        ORDER BY '],['+LTRIM([Language]) FOR XML PATH('') ),1,2,'') + ']'
SET @query=
'SELECT * FROM
  (SELECT YearBoughtIntoStore AS [Year Bought],[Language],NumberOfBooks
   FROM BookList) sourceData
PIVOT(SUM(NumberOfBooks)FOR [Language] IN ('+ @languages +')) pivotrReport' EXECUTE(@query)

```



## Simple Pivot - Static Columns


Using [Item Sales Table](http://stackoverflow.com/documentation/sql/280/example-database/1207/item-sales-table#t=201604231440127063045) from [Example Database](http://stackoverflow.com/documentation/sql/280/example-database#t=201604231440308642346), let us calculate and show the total Quantity we sold of each Product.

This can be easily done with a group by, but lets assume we to 'rotate' our result table in a way that for each Product Id we have a column.

```sql
SELECT [100], [145]
  FROM (SELECT ItemId , Quantity
          FROM #ItemSalesTable
       ) AS pivotIntermediate
 PIVOT (   SUM(Quantity)
           FOR ItemId IN ([100], [145])
       ) AS pivotTable

```

Since our 'new' columns are numbers (in the source table), we need to square brackets `[]`

This will give us an output like

|100|145
|---|---|---|---
|45|18



#### Syntax


<li>SELECT `<non-pivoted column>`,<br />
[first pivoted column] AS `<column name>`,<br />
[second pivoted column] AS `<column name>`,<br />
...<br />
[last pivoted column] AS `<column name>`<br />
FROM<br />
(`<SELECT query that produces the data>`)<br />
AS `<alias for the source query>`<br />
PIVOT<br />
(<br />
`<aggregation function>`(`<column being aggregated>`)<br />
FOR<br />
[`<column that contains the values that will become column headers>`]<br />
IN ( [first pivoted column], [second pivoted column],<br />
... [last pivoted column])<br />
) AS `<alias for the pivot table>`
`<optional ORDER BY clause>`;</li>



#### Remarks


Using PIVOT and UNPIVOT operators you transform a table by shifting the rows (column values) of a table to columns and vise-versa. As part of this transformation aggregation functions can be applied on the table values.

