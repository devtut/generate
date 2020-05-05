---
metaTitle: "Microsoft SQL Server - Modify JSON text"
description: "Modify value in JSON text on the specified path, Append a scalar value into a JSON array, Insert new JSON Object in JSON text, Insert new JSON array generated with FOR JSON query, Insert single JSON object generated with FOR JSON clause"
---

# Modify JSON text



## Modify value in JSON text on the specified path


JSON_MODIFY function uses JSON text as input parameter, and modifies a value on the specified path using third argument:

```sql
declare @json nvarchar(4000) = N'{"Id":1,"Name":"Toy Car","Price":34.99}'
set @json = JSON_MODIFY(@json, '$.Price', 39.99)
print @json -- Output: {"Id":1,"Name":"Toy Car","Price":39.99}

```

As a result, we will have new JSON text with "Price":39.99 and other value will not be changed. If object on the specified path does not exists, JSON_MODIFY will insert key:value pair.

In order to delete key:value pair, put NULL as new value:

```sql
declare @json nvarchar(4000) = N'{"Id":1,"Name":"Toy Car","Price":34.99}'
set @json = JSON_MODIFY(@json, '$.Price', NULL)
print @json -- Output: {"Id":1,"Name":"Toy Car"}

```

JSON_MODIFY will by default delete key if it does not have value so you can use it to delete a key.



## Append a scalar value into a JSON array


JSON_MODIFY has 'append' mode that appends value into array.

```sql
declare @json nvarchar(4000) = N'{"Id":1,"Name":"Toy Car","Tags":["toy","game"]}'
set @json = JSON_MODIFY(@json, 'append $.Tags', 'sales')
print @json -- Output: {"Id":1,"Name":"Toy Car","Tags":["toy","game","sales"]}

```

If array on the specified path does not exists, JSON_MODIFY(append) will create new array with a single element:

```sql
declare @json nvarchar(4000) = N'{"Id":1,"Name":"Toy Car","Price":34.99}'
set @json = JSON_MODIFY(@json, 'append $.Tags', 'sales')
print @json -- Output {"Id":1,"Name":"Toy Car","Tags":["sales"]}

```



## Insert new JSON Object in JSON text


JSON_MODIFY function enables you to insert JSON objects into JSON text:

```sql
declare @json nvarchar(4000) = N'{"Id":1,"Name":"Toy Car"}'
set @json = JSON_MODIFY(@json, '$.Price', 
                        JSON_QUERY('{"Min":34.99,"Recommended":45.49}'))
print @json
-- Output: {"Id":1,"Name":"Toy Car","Price":{"Min":34.99,"Recommended":45.49}}

```

Since third parameter is text you need to wrap it with JSON_QUERY function to "cast" text to JSON. Without this "cast", JSON_MODIFY will treat third parameter as plain text and escape characters before inserting it as string value. Without JSON_QUERY results will be:

```sql
{"Id":1,"Name":"Toy Car","Price":'{\"Min\":34.99,\"Recommended\":45.49}'}

```

JSON_MODIFY will insert this object if it does not exist, or delete it if value of third parameter is NULL.



## Insert new JSON array generated with FOR JSON query


You can generate JSON object using standard SELECT query with FOR JSON clause and insert it into JSON text as third parameter:

```sql
declare @json nvarchar(4000) = N'{"Id":17,"Name":"WWI"}'
set @json = JSON_MODIFY(@json, '$.tables', 
                        (select name from sys.tables FOR JSON PATH) )
print @json

(1 row(s) affected)
{"Id":1,"Name":"master","tables":[{"name":"Colors"},{"name":"Colors_Archive"},{"name":"OrderLines"},{"name":"PackageTypes"},{"name":"PackageTypes_Archive"},{"name":"StockGroups"},{"name":"StockItemStockGroups"},{"name":"StockGroups_Archive"},{"name":"StateProvinces"},{"name":"CustomerTransactions"},{"name":"StateProvinces_Archive"},{"name":"Cities"},{"name":"Cities_Archive"},{"name":"SystemParameters"},{"name":"InvoiceLines"},{"name":"Suppliers"},{"name":"StockItemTransactions"},{"name":"Suppliers_Archive"},{"name":"Customers"},{"name":"Customers_Archive"},{"name":"PurchaseOrders"},{"name":"Orders"},{"name":"People"},{"name":"StockItems"},{"name":"People_Archive"},{"name":"ColdRoomTemperatures"},{"name":"ColdRoomTemperatures_Archive"},{"name":"VehicleTemperatures"},{"name":"StockItems_Archive"},{"name":"Countries"},{"name":"StockItemHoldings"},{"name":"sysdiagrams"},{"name":"PurchaseOrderLines"},{"name":"Countries_Archive"},{"name":"DeliveryMethods"},{"name":"DeliveryMethods_Archive"},{"name":"PaymentMethods"},{"name":"SupplierTransactions"},{"name":"PaymentMethods_Archive"},{"name":"TransactionTypes"},{"name":"SpecialDeals"},{"name":"TransactionTypes_Archive"},{"name":"SupplierCategories"},{"name":"SupplierCategories_Archive"},{"name":"BuyingGroups"},{"name":"Invoices"},{"name":"BuyingGroups_Archive"},{"name":"CustomerCategories"},{"name":"CustomerCategories_Archive"}]}

```

JSON_MODIFY will know that select query with FOR JSON clause generates valid JSON array and it will just insert it into JSON text.

> 
<p>You can use all FOR JSON options in SELECT query, <strong>except
WITHOUT_ARRAY_WRAPPER</strong>, which will generate single object instead of
JSON array. See other example in this topic to see how insert single
JSON object.</p>




## Insert single JSON object generated with FOR JSON clause


You can generate JSON object using standard SELECT query with FOR JSON clause and WITHOUT_ARRAY_WRAPPER option, and insert it into JSON text as a third parameter:

```sql
declare @json nvarchar(4000) = N'{"Id":17,"Name":"WWI"}'
set @json = JSON_MODIFY(@json, '$.table', 
                        JSON_QUERY(
                         (select name, create_date, schema_id
                           from sys.tables
                           where name = 'Colors' 
                           FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)))
print @json

(1 row(s) affected)
{"Id":17,"Name":"WWI","table":{"name":"Colors","create_date":"2016-06-02T10:04:03.280","schema_id":13}}

```

FOR JSON with WITHOUT_ARRAY_WRAPPER option may generate invalid JSON text if SELECT query returns more than one result (you should use TOP 1 or filter by primary key in this case). Therefore, JSON_MODIFY will assume that returned result is just a plain text and escape it like any other text if you don't wrap it with JSON_QUERY function.

> 
<p>You should wrap **FOR JSON, WITHOUT_ARRAY_WRAPPER** query with
**JSON_QUERY** function in order to cast result to JSON.</p>


