---
metaTitle: "FOR JSON"
description: "FOR JSON PATH, FOR JSON PATH with column aliases, FOR JSON clause without array wrapper (single object in output), INCLUDE_NULL_VALUES, Wrapping results with ROOT object, FOR JSON AUTO, Creating custom nested JSON structure"
---

# FOR JSON



## FOR JSON PATH


Formats results of SELECT query as JSON text. FOR JSON PATH clause is added after query:

```sql
SELECT top 3 object_id, name, type, principal_id FROM sys.objects
FOR JSON PATH

```

Column names will be used as keys in JSON, and cell values will be generated as JSON values. Result of the query would be an array of JSON objects:

```sql
[
  {"object_id":3,"name":"sysrscols","type":"S "},       
  {"object_id":5,"name":"sysrowsets","type":"S "},
  {"object_id":6,"name":"sysclones","type":"S "}
]

```

NULL values in principal_id column will be ignored (they will not be generated).



## FOR JSON PATH with column aliases


FOR JSON PATH enables you to control format of the output JSON using column aliases:

```sql
SELECT top 3 object_id as id, name as [data.name], type as [data.type]
FROM sys.objects
FOR JSON PATH

```

Column alias will be used as a key name. Dot-separated column aliases (data.name and data.type) will be generated as nested objects. If two column have the same prefix in dot notation, they will be grouped together in single object (data in this example):

```sql
[
  {"id":3,"data":{"name":"sysrscols","type":"S "}},
  {"id":5,"data":{"name":"sysrowsets","type":"S "}},
  {"id":6,"data":{"name":"sysclones","type":"S "}}
]

```



## FOR JSON clause without array wrapper (single object in output)


WITHOUT_ARRAY_WRAPPER option enables you to generate a single object instead of the array. Use this option if you know that you will return single row/object:

```sql
SELECT top 3 object_id, name, type, principal_id
FROM sys.objects
WHERE object_id = 3
FOR JSON PATH, WITHOUT_ARRAY_WRAPPER

```

Single object will be returned in this case:

```sql
{"object_id":3,"name":"sysrscols","type":"S "}

```



## INCLUDE_NULL_VALUES


FOR JSON clause ignores NULL values in cells. If you want to generate "key": null pairs for cells that contain NULL values, add INCLUDE_NULL_VALUES option in the query:

```sql
SELECT top 3 object_id, name, type, principal_id
FROM sys.objects
FOR JSON PATH, INCLUDE_NULL_VALUES

```

NULL values in principal_id column will be generated:

```sql
[
  {"object_id":3,"name":"sysrscols","type":"S ","principal_id":null},
  {"object_id":5,"name":"sysrowsets","type":"S ","principal_id":null},
  {"object_id":6,"name":"sysclones","type":"S ","principal_id":null}
]

```



## Wrapping results with ROOT object


Wraps returned JSON array in additional root object with specified key:

```sql
SELECT top 3 object_id, name, type FROM sys.objects
FOR JSON PATH, ROOT('data')

```

Result of the query would be array of JSON objects inside the wrapper object:

```sql
{
  "data":[
           {"object_id":3,"name":"sysrscols","type":"S "},
           {"object_id":5,"name":"sysrowsets","type":"S "},
           {"object_id":6,"name":"sysclones","type":"S "}
         ]
}

```



## FOR JSON AUTO


Automatically nests values from the second table as a nested sub-array of JSON objects:

```sql
SELECT top 5 o.object_id, o.name, c.column_id, c.name
FROM sys.objects o
    JOIN sys.columns c ON o.object_id = c.object_id 
FOR JSON AUTO

```

Result of the query would be array of JSON objects:

```sql
[
  {
   "object_id":3,
   "name":"sysrscols",
   "c":[
        {"column_id":12,"name":"bitpos"},
        {"column_id":6,"name":"cid"}
       ]
  },
  {
    "object_id":5,
    "name":"sysrowsets",
    "c":[
         {"column_id":13,"name":"colguid"},
         {"column_id":3,"name":"hbcolid"},
         {"column_id":8,"name":"maxinrowlen"}
     ]
  }
]

```



## Creating custom nested JSON structure


If you need some complex JSON structure that cannot be created using FOR JSON PATH or FOR JSON AUTO, you can customize your JSON output by putting FOR JSON sub-queries as column expressions:

```sql
SELECT top 5 o.object_id, o.name,
        (SELECT column_id, c.name
            FROM sys.columns c WHERE o.object_id = c.object_id
            FOR JSON PATH) as columns,
        (SELECT parameter_id, name
            FROM sys.parameters p WHERE o.object_id = p.object_id
            FOR JSON PATH) as parameters
FROM sys.objects o
FOR JSON PATH

```

Each sub-query will produce JSON result that will be included in the main JSON content.

