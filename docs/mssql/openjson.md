---
metaTitle: "OPENJSON"
description: "Get key:value pairs from JSON text, Transform JSON array into set of rows, Transform nested JSON fields into set of rows, Extracting inner JSON sub-objects, Working with nested JSON sub-arrays"
---

# OPENJSON



## Get key:value pairs from JSON text


OPENJSON function parse JSON text and returns all key:value pairs at the first level of JSON:

```sql
declare @json NVARCHAR(4000) = N'{"Name":"Joe","age":27,"skills":["C#","SQL"]}';
SELECT * FROM OPENJSON(@json);

```

|key|value|type
|------
|Name|Joe|1
|age|27|2
|skills|["C#","SQL"]|4

Column type describe the type of value, i.e. null(0), string(1), number(2), boolean(3), array(4), and object(5).



## Transform JSON array into set of rows


OPENJSON function parses collection of JSON objects and returns values from JSON text as set of rows.

```sql
declare @json nvarchar(4000) = N'[
  {"Number":"SO43659","Date":"2011-05-31T00:00:00","Customer": "MSFT","Price":59.99,"Quantity":1},
  {"Number":"SO43661","Date":"2011-06-01T00:00:00","Customer":"Nokia","Price":24.99,"Quantity":3}
]'

SELECT    * 
FROM OPENJSON (@json)
    WITH (
          Number   varchar(200),
          Date     datetime,
          Customer varchar(200),
          Quantity int
  )

```

In the WITH clause is specified return schema of OPENJSON function. Keys in the JSON objects are fetched by column names. If some key in JSON is not specified in the WITH clause (e.g. Price in this example) it will be ignored. Values are automatically converted into specified types.

|Number|Date|Customer|Quantity
|------
|SO43659|2011-05-31T00:00:00|MSFT|1
|SO43661|2011-06-01T00:00:00|Nokia|3



## Transform nested JSON fields into set of rows


OPENJSON function parses collection of JSON objects and returns values from JSON text as set of rows. If the values in input object are nested, additional mapping parameter can be specified in each column in WITH clause:

```sql
declare @json nvarchar(4000) = N'[
  {"data":{"num":"SO43659","date":"2011-05-31T00:00:00"},"info":{"customer":"MSFT","Price":59.99,"qty":1}},
  {"data":{"number":"SO43661","date":"2011-06-01T00:00:00"},"info":{"customer":"Nokia","Price":24.99,"qty":3}}
]'

SELECT    * 
FROM OPENJSON (@json)
    WITH (
          Number   varchar(200) '$.data.num',
          Date     datetime '$.data.date',
          Customer varchar(200) '$.info.customer',
          Quantity int '$.info.qty',
  )

```

In the WITH clause is specified return schema of OPENJSON function. After the type is specified path to the JSON nodes where returned value should be found. Keys in the JSON objects are fetched by these paths. Values are automatically converted into specified types.

|Number|Date|Customer|Quantity
|------
|SO43659|2011-05-31T00:00:00|MSFT|1
|SO43661|2011-06-01T00:00:00|Nokia|3



## Extracting inner JSON sub-objects


OPENJSON can extract fragments of JSON objects inside the JSON text. In the column definition that references JSON sub-object set the type nvarchar(max) and AS JSON option:

```sql
declare @json nvarchar(4000) = N'[
  {"Number":"SO43659","Date":"2011-05-31T00:00:00","info":{"customer":"MSFT","Price":59.99,"qty":1}},
  {"Number":"SO43661","Date":"2011-06-01T00:00:00","info":{"customer":"Nokia","Price":24.99,"qty":3}}
]'

SELECT    * 
FROM OPENJSON (@json)
    WITH (
          Number   varchar(200),
          Date     datetime,
          Info nvarchar(max) '$.info' AS JSON
  )

```

Info column will be mapped to "Info" object. Results will be:

|Number|Date|Info
|------
|SO43659|2011-05-31T00:00:00|{"customer":"MSFT","Price":59.99,"qty":1}
|SO43661|2011-06-01T00:00:00|{"customer":"Nokia","Price":24.99,"qty":3}



## Working with nested JSON sub-arrays


JSON may have complex structure with inner arrays. In this example, we have array of orders with nested sub array of OrderItems.

```sql
declare @json nvarchar(4000) = N'[
  {"Number":"SO43659","Date":"2011-05-31T00:00:00",
    "Items":[{"Price":11.99,"Quantity":1},{"Price":12.99,"Quantity":5}]},
  {"Number":"SO43661","Date":"2011-06-01T00:00:00",
    "Items":[{"Price":21.99,"Quantity":3},{"Price":22.99,"Quantity":2},{"Price":23.99,"Quantity":2}]}
]'

```

We can parse root level properties using OPENJSON that will return Items array AS JSON fragment. Then we can apply OPENJSON again on Items array and open inner JSON table. First level table and inner table will be "joined" like in the JOIN between standard tables:

```sql
SELECT    * 
FROM
    OPENJSON (@json)
    WITH (  Number varchar(200), Date datetime,
            Items nvarchar(max) AS JSON )
        CROSS APPLY 
            OPENJSON (Items)
            WITH ( Price float, Quantity int)

```

Results:

|Number|Date|Items|Price|Quantity
|------
|SO43659|2011-05-31 00:00:00.000|[{"Price":11.99,"Quantity":1},{"Price":12.99,"Quantity":5}]|11.99|1
|SO43659|2011-05-31 00:00:00.000|[{"Price":11.99,"Quantity":1},{"Price":12.99,"Quantity":5}]|12.99|5
|SO43661|2011-06-01 00:00:00.000|[{"Price":21.99,"Quantity":3},{"Price":22.99,"Quantity":2},{"Price":23.99,"Quantity":2}]|21.99|3
|SO43661|2011-06-01 00:00:00.000|[{"Price":21.99,"Quantity":3},{"Price":22.99,"Quantity":2},{"Price":23.99,"Quantity":2}]|22.99|2
|SO43661|2011-06-01 00:00:00.000|[{"Price":21.99,"Quantity":3},{"Price":22.99,"Quantity":2},{"Price":23.99,"Quantity":2}]|23.99|2

