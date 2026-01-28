---
metaTitle: "MongoDB - Aggregation"
description: "Aggregate query examples useful for work and learning, Get sample data, Java and Spring example, Left Outer Join with aggregation ( $Lookup)"
---

# MongoDB Aggregation




## Aggregate query examples useful for work and learning


Aggregation is used to perform complex data search operations in the mongo query which can't be done in normal "find" query.

**Create some dummy data:**

```js
db.employees.insert({"name":"Adma","dept":"Admin","languages":["german","french","english","hindi"],"age":30, "totalExp":10});
db.employees.insert({"name":"Anna","dept":"Admin","languages":["english","hindi"],"age":35, "totalExp":11});
db.employees.insert({"name":"Bob","dept":"Facilities","languages":["english","hindi"],"age":36, "totalExp":14});
db.employees.insert({"name":"Cathy","dept":"Facilities","languages":["hindi"],"age":31, "totalExp":4});
db.employees.insert({"name":"Mike","dept":"HR","languages":["english", "hindi", "spanish"],"age":26, "totalExp":3});
db.employees.insert({"name":"Jenny","dept":"HR","languages":["english", "hindi", "spanish"],"age":25, "totalExp":3});

```

**Examples by topic:**

**1. Match:** Used to match documents (like SQL where clause)

```js
db.employees.aggregate([{$match:{dept:"Admin"}}]);
Output:
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "Admin", "languages" : [ "german", "french", "english", "hindi" ], "age" : 30, "totalExp" : 10 }
{ "_id" : ObjectId("54982fc92e9b4b54ec384a0e"), "name" : "Anna", "dept" : "Admin", "languages" : [ "english", "hindi" ], "age" : 35, "totalExp" : 11 }

```

**2. Project:** Used to populate specific field's value(s)

project stage will include _id field automatically unless you specify to disable.

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1}}]);
Output:
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "Admin" }
{ "_id" : ObjectId("54982fc92e9b4b54ec384a0e"), "name" : "Anna", "dept" : "Admin" }

db.employees.aggregate({$project: {'_id':0, 'name': 1}})
Output:
{ "name" : "Adma" }
{ "name" : "Anna" }
{ "name" : "Bob" }
{ "name" : "Cathy" }
{ "name" : "Mike" }
{ "name" : "Jenny" }

```

**3. Group:** $group is used to group documents by specific field, here documents are grouped by "dept" field's value.
Another useful feature is that you can group by null, it means all documents will be aggregated into one.

```js
db.employees.aggregate([{$group:{"_id":"$dept"}}]);                                                                            

{ "_id" : "HR" }                                                                                            
{ "_id" : "Facilities" }                                                                                             
{ "_id" : "Admin" } 

db.employees.aggregate([{$group:{"_id":null, "totalAge":{$sum:"$age"}}}]);
Output:
{ "_id" : null, "noOfEmployee" : 183 }

```

**4. Sum:** $sum is used to count or sum the values inside a group.

```js
db.employees.aggregate([{$group:{"_id":"$dept", "noOfDept":{$sum:1}}}]);
Output:
{ "_id" : "HR", "noOfDept" : 2 }
{ "_id" : "Facilities", "noOfDept" : 2 }
{ "_id" : "Admin", "noOfDept" : 2 }

```

**5. Average:** Calculates average of specific field's value per group.

```js
db.employees.aggregate([{$group:{"_id":"$dept", "noOfEmployee":{$sum:1}, "avgExp":{$avg:"$totalExp"}}}]);
Output: 
{ "_id" : "HR", "noOfEmployee" : 2, "totalExp" : 3 }
{ "_id" : "Facilities", "noOfEmployee" : 2, "totalExp" : 9 }
{ "_id" : "Admin", "noOfEmployee" : 2, "totalExp" : 10.5 }

```

**6. Minimum:** Finds minimum value of a field in each group.

```js
db.employees.aggregate([{$group:{"_id":"$dept", "noOfEmployee":{$sum:1}, "minExp":{$min:"$totalExp"}}}]);
Output: 
{ "_id" : "HR", "noOfEmployee" : 2, "totalExp" : 3 }
{ "_id" : "Facilities", "noOfEmployee" : 2, "totalExp" : 4 }
{ "_id" : "Admin", "noOfEmployee" : 2, "totalExp" : 10 }

```

**7. Maximum:** Finds maximum value of a field in each group.

```js
db.employees.aggregate([{$group:{"_id":"$dept", "noOfEmployee":{$sum:1}, "maxExp":{$max:"$totalExp"}}}]);
Output:
{ "_id" : "HR", "noOfEmployee" : 2, "totalExp" : 3 }
{ "_id" : "Facilities", "noOfEmployee" : 2, "totalExp" : 14 }
{ "_id" : "Admin", "noOfEmployee" : 2, "totalExp" : 11 }

```

**8. Getting specific field's value from first and last document of each group:** Works well when doucument result is sorted.

```js
db.employees.aggregate([{$group:{"_id":"$age", "lasts":{$last:"$name"}, "firsts":{$first:"$name"}}}]);
Output:
{ "_id" : 25, "lasts" : "Jenny", "firsts" : "Jenny" }
{ "_id" : 26, "lasts" : "Mike", "firsts" : "Mike" }
{ "_id" : 35, "lasts" : "Cathy", "firsts" : "Anna" }
{ "_id" : 30, "lasts" : "Adma", "firsts" : "Adma" }

```

**9. Minumum with maximum:**

```js
db.employees.aggregate([{$group:{"_id":"$dept", "noOfEmployee":{$sum:1}, "maxExp":{$max:"$totalExp"}, "minExp":{$min: "$totalExp"}}}]);
Output:
{ "_id" : "HR", "noOfEmployee" : 2, "maxExp" : 3, "minExp" : 3 }
{ "_id" : "Facilities", "noOfEmployee" : 2, "maxExp" : 14, "minExp" : 4 }
{ "_id" : "Admin", "noOfEmployee" : 2, "maxExp" : 11, "minExp" : 10 }

```

**10. Push and addToSet:** Push adds a field's value form each document in group to an array used to project data in array format, addToSet is simlar to push but it omits duplicate values.

```js
db.employees.aggregate([{$group:{"_id":"dept", "arrPush":{$push:"$age"}, "arrSet": {$addToSet:"$age"}}}]);
Output:
{ "_id" : "dept", "arrPush" : [ 30, 35, 35, 35, 26, 25 ], "arrSet" : [ 25, 26, 35, 30 ] }

```

**11. Unwind:** Used to create multiple in-memory documents for each value in the specified array type field, then we can do further aggregation based on those values.

```js
db.employees.aggregate([{$match:{"name":"Adma"}}, {$unwind:"$languages"}]);
Output: 
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "HR", "languages" : "german", "age" : 30, "totalExp" : 10 }
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "HR", "languages" : "french", "age" : 30, "totalExp" : 10 }
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "HR", "languages" : "english", "age" : 30, "totalExp" : 10 }
{ "_id" : ObjectId("54982fac2e9b4b54ec384a0d"), "name" : "Adma", "dept" : "HR", "languages" : "hindi", "age" : 30, "totalExp" : 10 }

```

**12. Sorting:**

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1}}, {$sort: {name: 1}}]);
Output:
{ "_id" : ObjectId("57ff3e553dedf0228d4862ac"), "name" : "Adma", "dept" : "Admin" }
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin" }

db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1}}, {$sort: {name: -1}}]);
Output:
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin" }
{ "_id" : ObjectId("57ff3e553dedf0228d4862ac"), "name" : "Adma", "dept" : "Admin" }

```

**13. Skip:**

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1}}, {$sort: {name: -1}}, {$skip:1}]);
Output:
{ "_id" : ObjectId("57ff3e553dedf0228d4862ac"), "name" : "Adma", "dept" : "Admin" }

```

**14. Limit:**

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1}}, {$sort: {name: -1}}, {$limit:1}]);  
Output:                                                                                                        
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin" }  

```

**15. Comparison operator in projection:**

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1, age: {$gt: ["$age", 30]}}}]);
Output:
{ "_id" : ObjectId("57ff3e553dedf0228d4862ac"), "name" : "Adma", "dept" : "Admin", "age" : false }
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin", "age" : true }

```

**16. Comparison operator in match:**

```js
db.employees.aggregate([{$match:{dept:"Admin", age: {$gt:30}}}, {$project:{"name":1, "dept":1}}]);   
Output:   
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin" }   

```

> 
List of comparison operators: $cmp, $eq, $gt, $gte, $lt, $lte, and $ne


**17. Boolean aggregation opertor in projection:**

```js
db.employees.aggregate([{$match:{dept:"Admin"}}, {$project:{"name":1, "dept":1, age: { $and: [ { $gt: [ "$age", 30 ] }, { $lt: [ "$age", 36 ] } ] }}}]);                                                                                
Output:
{ "_id" : ObjectId("57ff3e553dedf0228d4862ac"), "name" : "Adma", "dept" : "Admin", "age" : false }                   
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin", "age" : true }  

```

**18. Boolean aggregation opertor in match:**

```js
db.employees.aggregate([{$match:{dept:"Admin", $and: [{age: { $gt:  30 }}, {age: {$lt: 36 }} ] }}, {$project:{"name":1, "dept":1, age: { $and: [ { $gt: [ "$age", 30 ] }, { $lt: [ "$age", 36 ] } ] }}}]);                              
Output:
{ "_id" : ObjectId("57ff3e5e3dedf0228d4862ad"), "name" : "Anna", "dept" : "Admin", "age" : true }  

```

> 
List of boolean aggregation opertors: $and, $or, and $not.


Complete refrence: [https://docs.mongodb.com/v3.2/reference/operator/aggregation/](https://docs.mongodb.com/v3.2/reference/operator/aggregation/)



## Get sample data


To get random data from certain collection refer to `$sample` aggregation.

```js
db.emplyees.aggregate({ $sample: { size:1 } })

```

where `size` stands for number of items to select.



## Java and Spring example


This is an example code to create and execute the aggregate query in MongoDB using Spring Data.

```js
   try {
        MongoClient mongo = new MongoClient();
        DB db = mongo.getDB("so");
        DBCollection coll = db.getCollection("employees");

        //Equivalent to $match
        DBObject matchFields = new BasicDBObject();
        matchFields.put("dept", "Admin");
        DBObject match = new BasicDBObject("$match", matchFields);

        //Equivalent to $project
        DBObject projectFields = new BasicDBObject();
        projectFields.put("_id", 1);
        projectFields.put("name", 1);
        projectFields.put("dept", 1);
        projectFields.put("totalExp", 1);
        projectFields.put("age", 1);
        projectFields.put("languages", 1);
        DBObject project = new BasicDBObject("$project", projectFields);

        //Equivalent to $group
        DBObject groupFields = new BasicDBObject("_id", "$dept");
        groupFields.put("ageSet", new BasicDBObject("$addToSet", "$age"));
        DBObject employeeDocProjection = new BasicDBObject("$addToSet", new BasicDBObject("totalExp", "$totalExp").append("age", "$age").append("languages", "$languages").append("dept", "$dept").append("name", "$name"));
        groupFields.put("docs", employeeDocProjection);
        DBObject group = new BasicDBObject("$group", groupFields);

        //Sort results by age
        DBObject sort = new BasicDBObject("$sort", new BasicDBObject("age", 1));

        List<DBObject> aggregationList = new ArrayList<>();
        aggregationList.add(match);
        aggregationList.add(project);
        aggregationList.add(group);
        aggregationList.add(sort);
        AggregationOutput output = coll.aggregate(aggregationList);

        for (DBObject result : output.results()) {
            BasicDBList employeeList = (BasicDBList) result.get("docs");
            BasicDBObject employeeDoc = (BasicDBObject) employeeList.get(0);
            String name = employeeDoc.get("name").toString();
            System.out.println(name);
        }
    }catch (Exception ex){
        ex.printStackTrace();
    }

```

See the "resultSet" value in JSON format to understand the output format:

```js
[{
    "_id": "Admin",
    "ageSet": [35.0, 30.0],
    "docs": [{
        "totalExp": 11.0,
        "age": 35.0,
        "languages": ["english", "hindi"],
        "dept": "Admin",
        "name": "Anna"
    }, {
        "totalExp": 10.0,
        "age": 30.0,
        "languages": ["german", "french", "english", "hindi"],
        "dept": "Admin",
        "name": "Adma"
    }]
}]

```

The "resultSet" contains one entry for each group, "ageSet" contains the list of age of each employee of that group, "_id" contains the value of the field that is being used for grouping and "docs" contains data of each employee of that group that can be used in our own code and UI.



## Left Outer Join with aggregation ( $Lookup)


```js
let col_1 = db.collection('col_1');
let col_2 = db.collection('col_2');      
col_1 .aggregate([
    { $match: { "_id": 1 } },
    {
        $lookup: {
            from: "col_2",
            localField: "id",
            foreignField: "id",
            as: "new_document"
        }
    }
],function (err, result){
    res.send(result);
});

```

This feature was newly released in the mongodb **version 3.2** , which gives the user a stage to join one collection with the matching attributes from another collection

[Mongodb $LookUp documentation](https://docs.mongodb.com/manual/reference/operator/aggregation/lookup/)

