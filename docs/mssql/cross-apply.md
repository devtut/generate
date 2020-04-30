---
metaTitle: "cross apply"
description: "Join table rows with dynamically generated rows from a cell, Join table rows with JSON array stored in cell, Filter rows by array values"
---

# cross apply



## Join table rows with dynamically generated rows from a cell


CROSS APPLY enables you to "join" rows from a table with dynamically generated rows returned by some table-value function.

Imagine that you have a Company table with a column that contains an array of products (ProductList column), and a function that parse these values and returns a set of products. You can select all rows from a Company table, apply this function on a ProductList column and "join" generated results with parent Company row:

```sql
SELECT *
FROM Companies c 
     CROSS APPLY dbo.GetProductList( c.ProductList ) p

```

For each row, value of **ProductList** cell will be provided to the function, and the function will return those products as a set of rows that can be joined with the parent row.



## Join table rows with JSON array stored in cell


CROSS APPLY enables you to "join" rows from a table with collection of JSON objects stored in a column.

Imagine that you have a Company table with a column that contains an array of products (ProductList column) formatted as JSON array. OPENJSON table value function can parse these values and return the set of products. You can select all rows from a Company table, parse JSON products with OPENJSON and "join" generated results with parent Company row:

```sql
SELECT *
FROM Companies c 
     CROSS APPLY OPENJSON( c.ProductList )
                 WITH ( Id int, Title nvarchar(30), Price money)

```

For each row, value of **ProductList** cell will be provided to OPENJSON function that will transform JSON objects to rows with the schema defined in WITH clause.



## Filter rows by array values


If you store a list of tags in a row as coma separated values, **STRING_SPLIT** function enables you to transform list of tags into a table of values.
**CROSS APPLY** enables you to "join" values parsed by **STRING_SPLIT** function with a parent row.

Imagine that you have a Product table with a column that contains an array of comma separated tags (e.g. promo,sales,new). STRING_SPLIT and CROSS APPLY enable you to join product rows with their tags so you can filter products by tags:

```sql
SELECT *
FROM Products p 
     CROSS APPLY STRING_SPLIT( p.Tags, ',' ) tags
WHERE tags.value = 'promo'

```

For each row, value of **Tags** cell will be provided to STRING_SPLIT function that will return tag values. Then you can filter rows by these values.

**Note:** **STRING_SPLIT** function is not available before **SQL Server 2016**

