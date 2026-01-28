---
metaTitle: "Microsoft SQL Server - FOR XML PATH"
description: "Using FOR XML PATH to concatenate values, Specifying namespaces, Specifying structure using XPath expressions, Hello World XML"
---

# FOR XML PATH



## Using FOR XML PATH to concatenate values


The `FOR XML PATH` can be used for concatenating values into string. The example below concatenates values into a `CSV` string:

```sql
DECLARE @DataSource TABLE
(
    [rowID] TINYINT
   ,[FirstName] NVARCHAR(32)
);

INSERT INTO @DataSource ([rowID], [FirstName])
VALUES (1, 'Alex')
      ,(2, 'Peter')
      ,(3, 'Alexsandyr')
      ,(4, 'George');

SELECT STUFF
(
    (
        SELECT ',' + [FirstName]
        FROM @DataSource
        ORDER BY [rowID] DESC
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)')
    ,1
    ,1
    ,''
);

```

Few important notes:

- the `ORDER BY` clause can be used to order the values in a preferred way
- if a longer value is used as the concatenation separator, the `STUFF` function parameter must be changed too;

```sql
SELECT STUFF
(
    (
        SELECT '---' + [FirstName]
        FROM @DataSource
        ORDER BY [rowID] DESC
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)')
    ,1
    ,3 -- the "3" could also be represented as: LEN('---') for clarity
    ,''
);

```


- as the `TYPE` option and `.value` function are used, the concatenation works with `NVARCHAR(MAX)` string



## Specifying namespaces


```sql
WITH XMLNAMESPACES (
    DEFAULT 'http://www.w3.org/2000/svg',
    'http://www.w3.org/1999/xlink' AS xlink
)
SELECT 
    'example.jpg' AS 'image/@xlink:href',
    '50px' AS 'image/@width',
    '50px' AS 'image/@height'
FOR XML PATH('svg')

```

```sql
<svg xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="http://www.w3.org/2000/svg">
    <image xlink:href="firefox.jpg" width="50px" height="50px"/>
</svg>

```



## Specifying structure using XPath expressions


```sql
SELECT
    'XPath example' AS 'head/title',
    'This example demonstrates ' AS 'body/p',
    'https://www.w3.org/TR/xpath/' AS 'body/p/a/@href',
    'XPath expressions' AS 'body/p/a'
FOR XML PATH('html')

```

```sql
<html>
    <head>
        <title>XPath example</title>
    </head>
    <body>
        <p>This example demonstrates <a href="https://www.w3.org/TR/xpath/">XPath expressions</a></p>
    </body>
</html>

```

In `FOR XML PATH`, columns without a name become text nodes. `NULL` or `''` therefore become empty text nodes.
Note: you can convert a named column to an unnamed one by using `AS *`

```sql
DECLARE @tempTable TABLE (Ref INT, Des NVARCHAR(100), Qty INT)
INSERT INTO @tempTable VALUES (100001, 'Normal', 1), (100002, 'Foobar', 1), (100003, 'Hello World', 2)

SELECT ROW_NUMBER() OVER (ORDER BY Ref) AS '@NUM',
     'REF' AS 'FLD/@NAME', REF AS 'FLD', '',
     'DES' AS 'FLD/@NAME', DES AS 'FLD', '',
     'QTY' AS 'FLD/@NAME', QTY AS 'FLD'
FROM @tempTable 
FOR XML PATH('LIN'), ROOT('row')

```

```sql
<row>
  <LIN NUM="1">
    <FLD NAME="REF">100001</FLD>
    <FLD NAME="DES">Normal</FLD>
    <FLD NAME="QTY">1</FLD>
  </LIN>
  <LIN NUM="2">
    <FLD NAME="REF">100002</FLD>
    <FLD NAME="DES">Foobar</FLD>
    <FLD NAME="QTY">1</FLD>
  </LIN>
  <LIN NUM="3">
    <FLD NAME="REF">100003</FLD>
    <FLD NAME="DES">Hello World</FLD>
    <FLD NAME="QTY">2</FLD>
  </LIN>
</row>

```

Using (empty) text nodes helps to separate the previously output node from the next one, so that SQL Server knows to start a new element for the next column. Otherwise, it gets confused when the attribute already exists on what it thinks is the "current" element.

For example, without the the empty strings between the element and the attribute in the `SELECT` statement, SQL Server gives an error:

> 
Attribute-centric column 'FLD/@NAME' must not come after a non-attribute-centric sibling in XML hierarchy in FOR XML PATH.


Also note that this example also wrapped the XML in a root element named `row`, specified by `ROOT('row')`



## Hello World XML


```sql
SELECT 'Hello World' FOR XML PATH('example')

```

```sql
<example>Hello World</example>

```



#### Remarks


There are also several other `FOR XML` modes:

- `FOR XML RAW` - Creates one `<row>` element per row.
- `FOR XML AUTO` - Attempts to heuristically autogenerate a hierarchy.
- `FOR XML EXPLICIT` - Provides more control over the shape of the XML, but is more cumbersome than `FOR XML PATH`.

