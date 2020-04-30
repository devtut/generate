---
metaTitle: "Queries with JSON data"
description: "Using values from JSON in query, Using JSON values in reports, Filter-out bad JSON text from query results, Update value in JSON column, Append new value into JSON array, JOIN table with inner JSON collection, Finding rows that contain value in the JSON array"
---

# Queries with JSON data



## Using values from JSON in query


JSON_VALUE function enables you to take a data from JSON text on the path specified as the second argument, and use this value in any part of the select query:

```sql
select ProductID, Name, Color, Size, Price, JSON_VALUE(Data, '$.Type') as Type
from Product
where JSON_VALUE(Data, '$.Type') = 'part'

```



## Using JSON values in reports


Once JSON values are extracted from JSON text, you can use them ina any part of the query. You can create some kind of report on JSON data with grouping aggregations, etc:

```sql
select JSON_VALUE(Data, '$.Type') as type,
        AVG( cast(JSON_VALUE(Data, '$.ManufacturingCost') as float) ) as cost
from Product
group by JSON_VALUE(Data, '$.Type') 
having JSON_VALUE(Data, '$.Type') is not null

```



## Filter-out bad JSON text from query results


If some JSON text might not be properly formatted, you can remove those entries from query using ISJSON function.

```sql
select ProductID, Name, Color, Size, Price, JSON_VALUE(Data, '$.Type') as Type
from Product
where JSON_VALUE(Data, '$.Type') = 'part'
and ISJSON(Data) > 0

```



## Update value in JSON column


JSON_MODIFY function can be used to update value on some path. You can use this function to modify original value of JSON cell in UPDATE statement:

```sql
update Product
set Data = JSON_MODIFY(Data, '$.Price', 24.99)
where ProductID = 17;

```

JSON_MODIFY function will update or create Price key (if it does not exists). If new value is NULL, the key will be removed.
JSON_MODIFY function will treat new value as string (escape special characters, wrap it with double quotes to create proper JSON string). If your new value is JSON fragment, you should wrap it with JSON_QUERY function:

```sql
update Product
set Data = JSON_MODIFY(Data, '$.tags', JSON_QUERY('["promo","new"]'))
where ProductID = 17;

```

JSON_QUERY function without second parameter behaves like a "cast to JSON". Since the result of JSON_QUERY is valid JSON fragment (object or array), JSON_MODIFY will no escape this value when modifies input JSON.



## Append new value into JSON array


JSON_MODIFY function can be used to append new value to some array inside JSON:

```sql
update Product
set Data = JSON_MODIFY(Data, 'append $.tags', "sales")
where ProductID = 17;

```

New value will be appended at the end of the array, or a new array with value ["sales"] will be created.
JSON_MODIFY function will treat new value as string (escape special characters, wrap it with double quotes to create proper JSON string). If your new value is JSON fragment, you should wrap it with JSON_QUERY function:

```sql
update Product
set Data = JSON_MODIFY(Data, 'append $.tags', JSON_QUERY('{"type":"new"}'))
where ProductID = 17;

```

JSON_QUERY function without second parameter behaves like a "cast to JSON". Since the result of JSON_QUERY is valid JSON fragment (object or array), JSON_MODIFY will no escape this value when modifies input JSON.



## JOIN table with inner JSON collection


If you have a "child table" formatted as JSON collection and stored in-row as JSON column, you can unpack this collection, transform it to table and join it with parent row. Instead of the standard JOIN operator, you should use CROSS APPLY.
In this example, product parts are formatted as collection of JSON objects in and stored in Data column:

```sql
select ProductID, Name, Size, Price, Quantity, PartName, Code
from Product
    CROSS APPLY OPENJSON(Data, '$.Parts') WITH (PartName varchar(20), Code varchar(5))

```

Result of the query is equivalent to the join between Product and Part tables.



## Finding rows that contain value in the JSON array


In this example, Tags array may contain various keywords like ["promo", "sales"], so we can open this array and filter values:

```sql
select ProductID, Name, Color, Size, Price, Quantity
from Product
    CROSS APPLY OPENJSON(Data, '$.Tags') 
where value = 'sales'

```

OPENJSON will open inner collection of tags and return it as table. Then we can filter results by some value in the table.

