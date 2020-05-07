---
metaTitle: "MongoDB - Aggregation"
description: "Count, Sum, Average, Operations with arrays., Match, Remove docs that have a duplicate field in a collection (dedupe)"
---

# Aggregation


`Aggregations` operations process data records and return computed results. Aggregation operations group values from multiple documents together, and can perform a variety of operations on the grouped data to return a single result. MongoDB provides three ways to perform aggregation: the aggregation pipeline, the map-reduce function, and single purpose aggregation methods.

**From Mongo manual [https://docs.mongodb.com/manual/aggregation/](https://docs.mongodb.com/manual/aggregation/)**



## Count


How do you get the number of Debit and Credit transactions? One way to do it is by using `count()` function as below.

```js
> db.transactions.count({cr_dr : "D"});

```

or

```js
> db.transactions.find({cr_dr : "D"}).length();

```

But what if you do not know the possible values of `cr_dr` upfront. Here Aggregation framework comes to play. See the below Aggregate query.

```js
> db.transactions.aggregate( 
      [
          {
              $group : {
                  _id : '$cr_dr', // group by type of transaction
                // Add 1 for each document to the count for this type of transaction
                  count : {$sum : 1}
              }
          }
      ]
  );

```

And the result is

```js
{
    "_id" : "C",
    "count" : 3
}
{
    "_id" : "D",
    "count" : 5
}

```



## Sum


How to get the summation of `amount`? See the below aggregate query.

```js
> db.transactions.aggregate( 
     [
         {
             $group : {
                 _id : '$cr_dr',
                 count : {$sum : 1},    //counts the number
                 totalAmount : {$sum : '$amount'}    //sums the amount
             }
         }
     ] 
 );

```

And the result is

```js
{
    "_id" : "C",
    "count" : 3.0,
    "totalAmount" : 120.0
}
{
    "_id" : "D",
    "count" : 5.0,
    "totalAmount" : 410.0
}

```

Another version that sums `amount` and `fee`.

```js
> db.transactions.aggregate(
      [
          {
              $group : {
                  _id : '$cr_dr',
                  count : {$sum : 1},
                  totalAmount : {$sum : { $sum : ['$amount', '$fee']}}
              }
          }
      ] 
  );

```

And the result is

```js
{
    "_id" : "C",
    "count" : 3.0,
    "totalAmount" : 128.0
}    
{
    "_id" : "D",
    "count" : 5.0,
    "totalAmount" : 422.0
}

```



## Average


How to get the average amount of debit and credit transactions?

```js
> db.transactions.aggregate(
    [
        {
            $group : {
                _id : '$cr_dr', // group by type of transaction (debit or credit)
                count : {$sum : 1},    // number of transaction for each type
                totalAmount : {$sum : { $sum : ['$amount', '$fee']}},    // sum
                averageAmount : {$avg : { $sum : ['$amount', '$fee']}}   // average
            }
        }
    ] 
)

```

The result is

```js
{
    "_id" : "C", // Amounts for credit transactions
    "count" : 3.0,
    "totalAmount" : 128.0,
    "averageAmount" : 40.0
}
{
    "_id" : "D", // Amounts for debit transactions
    "count" : 5.0,
    "totalAmount" : 422.0,
    "averageAmount" : 82.0
}

```



## Operations with arrays.


When you want to work with the data entries in arrays you first need to [unwind](https://docs.mongodb.com/manual/reference/operator/aggregation/unwind/) the array. The unwind operation creates a document for each entry in the array. When you have lot's of documents with large arrays you will see an explosion in number of documents.

```js
{ "_id" : 1, "item" : "myItem1", sizes: [ "S", "M", "L"] }
{ "_id" : 2, "item" : "myItem2", sizes: [ "XS", "M", "XL"] }
    
db.inventory.aggregate( [ { $unwind : "$sizes" }] )

```

An important notice is that when a document doesn't contain the array it will be lost. From mongo 3.2 and up there are is an unwind option "preserveNullAndEmptyArrays" added. This option makes sure the document is preserved when the array is missing.

```js
{ "_id" : 1, "item" : "myItem1", sizes: [ "S", "M", "L"] }
{ "_id" : 2, "item" : "myItem2", sizes: [ "XS", "M", "XL"] }
{ "_id" : 3, "item" : "myItem3" }
    
db.inventory.aggregate( [ { $unwind : { path: "$sizes", includeArrayIndex: "arrayIndex" } }] )

```



## Match


How to write a query to get all departments where average age of employees making less than or $70000 is greather than or equal to 35?

In order to that we need to write a query to match employees that have a salary that is less than or equal to $70000. Then add the aggregate stage to group the employees by the department. Then add an accumulator with a field named e.g. average_age to find the average age per department using the $avg accumulator and below the existing $match and $group aggregates add another $match aggregate so that we're only retrieving results with an average_age that is greather than or equal to 35.

```js
db.employees.aggregate([
  {"$match": {"salary": {"$lte": 70000}}},
  {"$group": {"_id": "$dept",
              "average_age": {"$avg": "$age"}
             }
  },
  {"$match": {"average_age": {"$gte": 35}}}
])

```

The result is:

```js
{
  "_id": "IT",
  "average_age": 31
}
{
  "_id": "Customer Service",
  "average_age": 34.5
}
{
  "_id": "Finance",
  "average_age": 32.5
}

```



## Remove docs that have a duplicate field in a collection (dedupe)


Note that the allowDiskUse: true option is optional but will help mitigate out of memory issues as this aggregation can be a memory intensive operation if your collection size is large - so i recommend to always use it.

```js
var duplicates = [];

db.transactions.aggregate([
 { $group: { 
   _id: { cr_dr: "$cr_dr"},
   dups: { "$addToSet": "$_id" }, 
   count: { "$sum": 1 } 
 }
}, 
{ $match: { 
  count: { "$gt": 1 }   
}}
],allowDiskUse: true}
)              
.result          
.forEach(function(doc) {
  doc.dups.shift();   
  doc.dups.forEach( function(dupId){ 
    duplicates.push(dupId); 
  }
)    
})
// printjson(duplicates);     

// Remove all duplicates in one go    
db.transactions.remove({_id:{$in:duplicates}})

```



#### Syntax


- **db.collection.aggregate(pipeline, options)**



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|**pipeline**|array(A sequence of data aggregation operations or stages)
|**options**|document(optional, available only if pipeline present as an array)



#### Remarks


Aggregation framework in MongoDB is used to achieve common `GROUP BY` functionality of SQL.

Consider the following insertions in collection named `transactions` for every example.

```js
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 100, fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 10,  fee : 2});
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 4});
> db.transactions.insert({ cr_dr : "D", amount : 10,  fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 10,  fee : 4});
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 2});

```

