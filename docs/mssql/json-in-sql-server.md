---
metaTitle: "JSON in Sql Server"
description: "Format Query Results as JSON with FOR JSON, Parse JSON text, Join parent and child JSON entities using CROSS APPLY OPENJSON, Index on JSON properties by using computed columns, Format one table row as a single JSON object using FOR JSON, Parse JSON text using OPENJSON function"
---

# JSON in Sql Server



## Format Query Results as JSON with FOR JSON


Input table data (People table)

|Id|Name|Age
|---|---|---|---|---|---
|1|John|23
|2|Jane|31

Query

```sql
SELECT Id, Name, Age
FROM People
FOR JSON PATH

```

Result

```sql
[
    {"Id":1,"Name":"John","Age":23},
    {"Id":2,"Name":"Jane","Age":31}
]

```



## Parse JSON text


**JSON_VALUE** and **JSON_QUERY** functions parse JSON text and return scalar values or objects/arrays on the path in JSON text.

```sql
DECLARE @json NVARCHAR(100) = '{"id": 1, "user":{"name":"John"}, "skills":["C#","SQL"]}'

SELECT
    JSON_VALUE(@json, '$.id') AS Id,
    JSON_VALUE(@json, '$.user.name') AS Name,
    JSON_QUERY(@json, '$.user') AS UserObject,
    JSON_QUERY(@json, '$.skills') AS Skills,
    JSON_VALUE(@json, '$.skills[0]') AS Skill0

```

**Result**

|Id|Name|UserObject|Skills|Skill0
|---|---|---|---|---|---
|1|John|{"name":"John"}|["C#","SQL"]|C#



## Join parent and child JSON entities using CROSS APPLY OPENJSON


Join parent objects with their child entities, for example we want a relational table of each person and their hobbies

```sql
DECLARE @json nvarchar(1000) =
N'[
    {
        "id":1,
        "user":{"name":"John"},
        "hobbies":[
            {"name": "Reading"},
            {"name": "Surfing"}
        ]
    },
    {
        "id":2,
        "user":{"name":"Jane"},
        "hobbies":[
            {"name": "Programming"},
            {"name": "Running"}
        ]
    }
 ]'

```

**Query**

```sql
SELECT 
    JSON_VALUE(person.value, '$.id') as Id,
    JSON_VALUE(person.value, '$.user.name') as PersonName,
    JSON_VALUE(hobbies.value, '$.name') as Hobby
FROM OPENJSON (@json) as person
    CROSS APPLY OPENJSON(person.value, '$.hobbies') as hobbies

```

Alternatively this query can be written using the WITH clause.

```sql
SELECT 
    Id, person.PersonName, Hobby
FROM OPENJSON (@json)
WITH(
    Id int '$.id',
    PersonName nvarchar(100) '$.user.name',
    Hobbies nvarchar(max) '$.hobbies' AS JSON
) as person
CROSS APPLY OPENJSON(Hobbies)
WITH(
    Hobby nvarchar(100) '$.name'
)

```

**Result**

|Id|PersonName|Hobby
|---|---|---|---|---|---
|1|John|Reading
|1|John|Surfing
|2|Jane|Programming
|2|Jane|Running



## Index on JSON properties by using computed columns


When storing JSON documents in SQL Server, We need to be able to efficiently filter and sort query results on properties of the JSON documents.

```sql
CREATE TABLE JsonTable
(
    id int identity primary key,
    jsonInfo nvarchar(max),
    CONSTRAINT [Content should be formatted as JSON]
    CHECK (ISJSON(jsonInfo)>0)
)

```

```sql
INSERT INTO JsonTable
VALUES(N'{"Name":"John","Age":23}'),
(N'{"Name":"Jane","Age":31}'),
(N'{"Name":"Bob","Age":37}'),
(N'{"Name":"Adam","Age":65}')
GO

```

Given the above table If we want to find the row with the name = 'Adam', we would execute the following query.

```sql
SELECT * 
FROM JsonTable Where 
JSON_VALUE(jsonInfo, '$.Name') = 'Adam'

```

However this will require SQL server to perform a full table which on a large table is not efficent.

To speed this up we would like to add an index, however we cannot directly reference properties in the JSON document. The solution is to add a computed column on the JSON path `$.Name`, then add an index on the computed column.

```sql
ALTER TABLE JsonTable
ADD vName as JSON_VALUE(jsonInfo, '$.Name')

CREATE INDEX idx_name
ON JsonTable(vName)

```

Now when we execute the same query, instead of a full table scan SQL server uses an index to seek into the non-clustered index and find the rows that satisfy the specified conditions.

Note: For SQL server to use the index, you must create the computed column with the same expression that you plan to use in your queries - in this example `JSON_VALUE(jsonInfo, '$.Name')`, however you can also use the name of computed column `vName`



## Format one table row as a single JSON object using FOR JSON


**WITHOUT_ARRAY_WRAPPER** option in **FOR JSON** clause will remove array brackets from the JSON output. This is useful if you are returning single row in the query.

> 
Note: this option will produce invalid JSON output if more than one row is returned.


Input table data (People table)

|Id|Name|Age
|---|---|---|---|---|---
|1|John|23
|2|Jane|31

Query

```sql
SELECT Id, Name, Age
FROM People
WHERE Id = 1
FOR JSON PATH, WITHOUT_ARRAY_WRAPPER

```

Result

```sql
{"Id":1,"Name":"John","Age":23}

```



## Parse JSON text using OPENJSON function


**OPENJSON** function parses JSON text and returns multiple outputs. Values that should be returned are specified using the paths defined in the WITH clause. If a path is not specified for some column, the column name is used as a path. This function casts returned values to the SQL types defined in the WITH clause. AS JSON option must be specified in the column definition if some object/array should be returned.

```sql
DECLARE @json NVARCHAR(100) = '{"id": 1, "user":{"name":"John"}, "skills":["C#","SQL"]}'

SELECT * 
FROM OPENJSON (@json)
    WITH(Id int '$.id',
        Name nvarchar(100) '$.user.name',
        UserObject nvarchar(max) '$.user' AS JSON,
        Skills nvarchar(max) '$.skills' AS JSON,
        Skill0 nvarchar(20) '$.skills[0]')

```

**Result**

|Id|Name|UserObject|Skills|Skill0
|---|---|---|---|---|---
|1|John|{"name":"John"}|["C#","SQL"]|C#



#### Syntax


- **JSON_VALUE**(expression , path) -- extract a scalar value from a JSON string.
- **JSON_QUERY**( expression [ , path ] ) -- Extracts an object or an array from a JSON string.
- **OPENJSON**( jsonExpression [ , path ] ) -- table-value function that parses JSON text and returns objects and properties in JSON as rows and columns.
- **ISJSON**( expression ) -- Tests whether a string contains valid JSON.
- **JSON_MODIFY**( expression , path , newValue ) -- Updates the value of a property in a JSON string and returns the updated JSON string.



#### Parameters


|Parameters|Details
|---|---|---|---|---|---
|expression|Typically the name of a variable or a column that contains JSON text.
|path|A JSON path expression that specifies the property to update. path has the following syntax: [append] [ lax | strict ] $.<json path>
|jsonExpression|Is a Unicode character expression containing the JSON text.



#### Remarks


The OPENJSON function is only available under compatibility level 130. If your database compatibility level is lower than 130, SQL Server will not be able to find and execute OPENJSON function. Currently all Azure SQL databases are set to 120 by default.
You can change the compatibility level of a database using the following command:

```sql
ALTER DATABASE <Database-Name-Here> SET COMPATIBILITY_LEVEL = 130

```

