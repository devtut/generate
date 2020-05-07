---
metaTitle: "MongoDB - Querying for Data ( Getting Started )"
description: "Find(), FindOne(), limit, skip, sort and count the results of the find() method, Query Document - Using AND, OR and IN Conditions, find() method with Projection, Find() method with Projection"
---

# Querying for Data ( Getting Started )


Basic querying examples



## Find()


**retrieve all documents in a collection**

```js
db.collection.find({});

```

**retrieve documents in a collection using a condition ( similar to WHERE in MYSQL )**

```js
db.collection.find({key: value}); 
example
  db.users.find({email:"sample@email.com"});

```

**retrieve documents in a collection using Boolean conditions (Query Operators)**

```js
//AND
db.collection.find( {
    $and: [
     { key: value }, { key: value } 
    ] 
})
//OR
db.collection.find( {
    $or: [
     { key: value }, { key: value } 
    ] 
})
//NOT
db.inventory.find( { key: { $not: value } } )

```

more boolean operations and examples can be found [here](https://docs.mongodb.com/manual/reference/operator/query/#query-and-projection-operators)

**NOTE:** **find()** will keep on searching the collection even if a document match has been found , therefore it is inefficient when used in a large collection , however by carefully modeling your data and/or using indexes you can increase the efficiency of **find()**



## FindOne()


```js
db.collection.findOne({});

```

the querying functionality is similar to find() but this will end execution the moment it finds one document matching its condition , if used with and empty object , it will fetch the first document and return it .
[findOne() mongodb api documentation](http://mongodb.github.io/node-mongodb-native/2.0/api/Collection.html#findOne)



## limit, skip, sort and count the results of the find() method


Similar to aggregation methods also by the find() method you have the possibility to limit, skip, sort and count the results. Let say we have following collection:

```js
db.test.insertMany([
    {name:"Any", age:"21", status:"busy"}, 
    {name:"Tony", age:"25", status:"busy"}, 
    {name:"Bobby", age:"28", status:"online"}, 
    {name:"Sonny", age:"28", status:"away"}, 
    {name:"Cher", age:"20", status:"online"}
])

```

To list the collection:

```js
db.test.find({})

```

Will return:

```js
{ "_id" : ObjectId("592516d7fbd5b591f53237b0"), "name" : "Any", "age" : "21", "status" : "busy" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b1"), "name" : "Tony", "age" : "25", "status" : "busy" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b2"), "name" : "Bobby", "age" : "28", "status" : "online" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b3"), "name" : "Sonny", "age" : "28", "status" : "away" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b4"), "name" : "Cher", "age" : "20", "status" : "online" }

```

To skip first 3 documents:

```js
db.test.find({}).skip(3)

```

Will return:

```js
{ "_id" : ObjectId("592516d7fbd5b591f53237b3"), "name" : "Sonny", "age" : "28", "status" : "away" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b4"), "name" : "Cher", "age" : "20", "status" : "online" }

```

To sort descending by the field name:

```js
db.test.find({}).sort({ "name" : -1})

```

Will return:

```js
{ "_id" : ObjectId("592516d7fbd5b591f53237b1"), "name" : "Tony", "age" : "25", "status" : "busy" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b3"), "name" : "Sonny", "age" : "28", "status" : "away" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b4"), "name" : "Cher", "age" : "20", "status" : "online" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b2"), "name" : "Bobby", "age" : "28", "status" : "online" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b0"), "name" : "Any", "age" : "21", "status" : "busy" }

```

If you want to sort ascending just replace -1 with 1

To count the results:

```js
db.test.find({}).count()

```

Will return:

```js
5

```

Also combinations of this methods are allowed. For example get 2 documents from descending sorted collection skipping the first 1:

```js
db.test.find({}).sort({ "name" : -1}).skip(1).limit(2)

```

Will return:

```js
{ "_id" : ObjectId("592516d7fbd5b591f53237b3"), "name" : "Sonny", "age" : "28", "status" : "away" }
{ "_id" : ObjectId("592516d7fbd5b591f53237b4"), "name" : "Cher", "age" : "20", "status" : "online" }

```



## Query Document - Using AND, OR and IN Conditions


All documents from `students` collection.

```js
> db.students.find().pretty();

{
    "_id" : ObjectId("58f29a694117d1b7af126dca"),
    "studentNo" : 1,
    "firstName" : "Prosen",
    "lastName" : "Ghosh",
    "age" : 25
}
{
    "_id" : ObjectId("58f29a694117d1b7af126dcb"),
    "studentNo" : 2,
    "firstName" : "Rajib",
    "lastName" : "Ghosh",
    "age" : 25
}
{
    "_id" : ObjectId("58f29a694117d1b7af126dcc"),
    "studentNo" : 3,
    "firstName" : "Rizve",
    "lastName" : "Amin",
    "age" : 23
}
{
    "_id" : ObjectId("58f29a694117d1b7af126dcd"),
    "studentNo" : 4,
    "firstName" : "Jabed",
    "lastName" : "Bangali",
    "age" : 25
}
{
    "_id" : ObjectId("58f29a694117d1b7af126dce"),
    "studentNo" : 5,
    "firstName" : "Gm",
    "lastName" : "Anik",
    "age" : 23
}

```

> 
Similar `mySql` Query of the above command.

```js
SELECT * FROM students;

```




```js
db.students.find({firstName:"Prosen"});

{ "_id" : ObjectId("58f2547804951ad51ad206f5"), "studentNo" : "1", "firstName" : "Prosen", "lastName" : "Ghosh", "age" : "23" }

```

> 
Similar `mySql` Query of the above command.

```js
SELECT * FROM students WHERE firstName = "Prosen";

```




**AND Queries**

```js
db.students.find({
    "firstName": "Prosen",
    "age": {
        "$gte": 23
    }
});

{ "_id" : ObjectId("58f29a694117d1b7af126dca"), "studentNo" : 1, "firstName" : "Prosen", "lastName" : "Ghosh", "age" : 25 }

```

> 
Similar `mySql` Query of the above command.

```js
SELECT * FROM students WHERE firstName = "Prosen" AND age >= 23

```




**Or Queries**

```js
db.students.find({
     "$or": [{
         "firstName": "Prosen"
     }, {
         "age": {
             "$gte": 23
         }
     }]
 });

{ "_id" : ObjectId("58f29a694117d1b7af126dca"), "studentNo" : 1, "firstName" : "Prosen", "lastName" : "Ghosh", "age" : 25 }
{ "_id" : ObjectId("58f29a694117d1b7af126dcb"), "studentNo" : 2, "firstName" : "Rajib", "lastName" : "Ghosh", "age" : 25 }
{ "_id" : ObjectId("58f29a694117d1b7af126dcc"), "studentNo" : 3, "firstName" : "Rizve", "lastName" : "Amin", "age" : 23 }
{ "_id" : ObjectId("58f29a694117d1b7af126dcd"), "studentNo" : 4, "firstName" : "Jabed", "lastName" : "Bangali", "age" : 25 }
{ "_id" : ObjectId("58f29a694117d1b7af126dce"), "studentNo" : 5, "firstName" : "Gm", "lastName" : "Anik", "age" : 23 }

```

> 
Similar `mySql` Query of the above command.

```js
SELECT * FROM students WHERE firstName = "Prosen" OR age >= 23

```




**And OR Queries**

```js
db.students.find({
        firstName : "Prosen",
        $or : [
            {age : 23},
            {age : 25}
        ]
});

{ "_id" : ObjectId("58f29a694117d1b7af126dca"), "studentNo" : 1, "firstName" : "Prosen", "lastName" : "Ghosh", "age" : 25 }

```

> 
Similar mySql Query of the above command.

```js
SELECT * FROM students WHERE firstName = "Prosen" AND age = 23 OR age = 25;

```




**IN Queries**
This queries can improve multiple use of OR Queries

```js
db.students.find(lastName:{$in:["Ghosh", "Amin"]})

{ "_id" : ObjectId("58f29a694117d1b7af126dca"), "studentNo" : 1, "firstName" : "Prosen", "lastName" : "Ghosh", "age" : 25 }
{ "_id" : ObjectId("58f29a694117d1b7af126dcb"), "studentNo" : 2, "firstName" : "Rajib", "lastName" : "Ghosh", "age" : 25 }
{ "_id" : ObjectId("58f29a694117d1b7af126dcc"), "studentNo" : 3, "firstName" : "Rizve", "lastName" : "Amin", "age" : 23 }

```

> 
Similar mySql query to above command

```js
select * from students where lastName in ('Ghosh', 'Amin')

```






## find() method with Projection


The basic syntax of `find()` method with projection is as follows

```js
> db.COLLECTION_NAME.find({},{KEY:1});

```

If you want to show all documents without the age field then the command is as follows

```js
db.people.find({},{age : 0});

```

If you want to show all documents the age field then the command is as follows



## Find() method with Projection


In MongoDB, projection means selecting only the necessary data rather than selecting whole of the data of a document.

The basic syntax of `find()` method with projection is as follows

```js
> db.COLLECTION_NAME.find({},{KEY:1});

```

If you want to to show all document without the age field then the command is as follows

```js
> db.people.find({},{age:0});

```

If you want to show only the age field then the command is as follows

```js
> db.people.find({},{age:1});

```

**Note:** `_id` field is always displayed while executing `find()` method, if you don't want this field, then you need to set it as `0`.

```js
> db.people.find({},{name:1,_id:0});

```

**Note:** `1` is used to show the field while `0` is used to hide the fields.

